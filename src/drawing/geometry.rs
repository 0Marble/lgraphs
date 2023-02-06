use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec2 {
    pub x: f32,
    pub y: f32,
}

impl Mul<f32> for Vec2 {
    type Output = Vec2;

    fn mul(self, rhs: f32) -> Self::Output {
        Vec2::new(self.x * rhs, self.y * rhs)
    }
}
impl Div<f32> for Vec2 {
    type Output = Vec2;

    fn div(self, rhs: f32) -> Self::Output {
        Vec2::new(self.x / rhs, self.y / rhs)
    }
}
impl Add<Vec2> for Vec2 {
    type Output = Vec2;

    fn add(self, rhs: Vec2) -> Self::Output {
        Vec2::new(self.x + rhs.x, self.y + rhs.y)
    }
}
impl Sub<Vec2> for Vec2 {
    type Output = Vec2;

    fn sub(self, rhs: Vec2) -> Self::Output {
        Vec2::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl Vec2 {
    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
    pub fn dot(&self, other: &Self) -> f32 {
        self.x * other.x + self.y * other.y
    }
    pub fn len_sqared(&self) -> f32 {
        self.dot(self)
    }
    pub fn len(&self) -> f32 {
        self.len_sqared().sqrt()
    }
    pub fn normalize(&self) -> Self {
        let len = self.len();
        Self {
            x: self.x / len,
            y: self.y / len,
        }
    }
    pub fn normal(&self) -> Self {
        Self {
            x: self.y,
            y: -self.x,
        }
    }
    pub fn rotate(&self, angle: f32) -> Vec2 {
        Vec2::new(
            angle.cos() * self.x - angle.sin() * self.y,
            angle.sin() * self.x + angle.cos() * self.y,
        )
    }
    pub fn as_tuple(&self) -> (f32, f32) {
        (self.x, self.y)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rect {
    pub x1: f32,
    pub y1: f32,
    pub x2: f32,
    pub y2: f32,
}

impl Rect {
    pub fn new(x1: f32, y1: f32, x2: f32, y2: f32) -> Self {
        Self {
            x1: f32::min(x1, x2),
            y1: f32::min(y1, y2),
            x2: f32::max(x1, x2),
            y2: f32::max(y1, y2),
        }
    }
    pub fn width(&self) -> f32 {
        self.x2 - self.x1
    }
    pub fn height(&self) -> f32 {
        self.y2 - self.y1
    }
    pub fn uv(&self, u: f32, v: f32) -> Vec2 {
        let x = Vec2::new(self.width(), 0.0);
        let y = Vec2::new(0.0, self.height());

        Vec2::new(self.x1, self.y1) + x * u + y * v
    }
    pub fn center(&self) -> Vec2 {
        Vec2::new((self.x1 + self.x2) * 0.5, (self.y1 + self.y2) * 0.5)
    }
    pub fn rect_at(&self, width: f32, height: f32, u: f32, v: f32) -> Rect {
        let pt = self.uv(u, v);
        Rect::new(
            pt.x - width * 0.5,
            pt.y - height * 0.5,
            pt.x + width * 0.5,
            pt.y + height * 0.5,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Circle {
    pub x: f32,
    pub y: f32,
    pub r: f32,
}

impl Circle {
    pub fn new(x: f32, y: f32, r: f32) -> Self {
        Self { x, y, r }
    }
    pub fn polar(&self, r: f32, theta: f32) -> Vec2 {
        let x = f32::cos(theta) * r * self.r;
        let y = f32::sin(theta) * r * self.r;
        self.center() + Vec2::new(x, y)
    }
    pub fn center(&self) -> Vec2 {
        Vec2::new(self.x, self.y)
    }
    pub fn rect_at(&self, width: f32, height: f32, r: f32, theta: f32) -> Rect {
        let pt = self.polar(r, theta);
        Rect::new(
            pt.x - width * 0.5,
            pt.y - height * 0.5,
            pt.x + width * 0.5,
            pt.y + height * 0.5,
        )
    }
    pub fn bounds(&self) -> Rect {
        Rect::new(
            self.x - self.r,
            self.y - self.r,
            self.x + self.r,
            self.y + self.r,
        )
    }
    pub fn to_point(&self, point: Vec2) -> Curve {
        let dir = point - self.center();
        let start = self.center() + dir.normalize() * self.r;
        Curve::straight(start, point)
    }
    pub fn line_between(&self, other: Self, droopiness: f32) -> Curve {
        let dir = (other.center() - self.center()).normalize();
        let start = self.center() + dir * self.r;
        let end = other.center() - dir * other.r;

        let normal = dir.normal();
        let len = self.dist(other);
        let p2 = start + dir * len * 0.25 + normal * droopiness;
        let p3 = start + dir * len * 0.75 + normal * droopiness;
        Curve::from_points(start, p2, p3, end)
    }
    pub fn dist(&self, other: Self) -> f32 {
        (self.center() - other.center()).len() - self.r - other.r
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Curve {
    pub p1: Vec2,
    pub p2: Vec2,
    pub p3: Vec2,
    pub p4: Vec2,
}

impl Curve {
    pub fn straight(from: Vec2, to: Vec2) -> Self {
        let p1 = from;
        let p4 = to;
        let p2 = p1 + (p4 - p1) * 0.25;
        let p3 = p1 + (p4 - p1) * 0.75;
        Self::from_points(p1, p2, p3, p4)
    }

    pub fn from_points(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2) -> Self {
        Self { p1, p2, p3, p4 }
    }

    pub fn t(&self, t: f32) -> Vec2 {
        self.p1 * (1.0 - t) * (1.0 - t) * (1.0 - t)
            + self.p2 * 3.0 * (1.0 - t) * (1.0 - t) * t
            + self.p3 * 3.0 * t * t * (1.0 - t)
            + self.p4 * t * t * t
    }

    pub fn bounds(&self) -> Rect {
        Rect::new(
            [self.p1.x, self.p2.x, self.p3.x, self.p4.x]
                .into_iter()
                .reduce(|a, b| if a < b { a } else { b })
                .unwrap(),
            [self.p1.y, self.p2.y, self.p3.y, self.p4.y]
                .into_iter()
                .reduce(|a, b| if a < b { a } else { b })
                .unwrap(),
            [self.p1.x, self.p2.x, self.p3.x, self.p4.x]
                .into_iter()
                .reduce(|a, b| if a > b { a } else { b })
                .unwrap(),
            [self.p1.y, self.p2.y, self.p3.y, self.p4.y]
                .into_iter()
                .reduce(|a, b| if a > b { a } else { b })
                .unwrap(),
        )
    }

    pub fn rect_at(&self, width: f32, height: f32, t: f32) -> Rect {
        let pt = self.t(t);
        Rect::new(
            pt.x - width * 0.5,
            pt.y - height * 0.5,
            pt.x + width * 0.5,
            pt.y + height * 0.5,
        )
    }

    pub fn start(&self) -> Vec2 {
        self.p1
    }

    pub fn end(&self) -> Vec2 {
        self.p4
    }

    pub fn reverse(&self) -> Curve {
        Curve {
            p1: self.p4,
            p2: self.p3,
            p3: self.p2,
            p4: self.p1,
        }
    }
}
