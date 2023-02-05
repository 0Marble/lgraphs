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
    pub fn center_rect(&self, width: f32, height: f32) -> Rect {
        let dw = self.width() - width;
        let dh = self.height() - height;

        Rect::new(
            self.x1 + dw * 0.5,
            self.y1 + dh * 0.5,
            self.x2 - dw * 0.5,
            self.y2 - dh * 0.5,
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
        let x = f32::cos(theta) * r;
        let y = f32::sin(theta) * r;
        self.center() + Vec2::new(x, y)
    }
    pub fn center(&self) -> Vec2 {
        Vec2::new(self.x, self.y)
    }
    pub fn center_rect(&self, width: f32, height: f32) -> Rect {
        Rect::new(
            self.center().x - width * 0.5,
            self.center().y - height * 0.5,
            self.center().x + width * 0.5,
            self.center().y + height * 0.5,
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
    pub fn to_point(&self, point: Vec2) -> Line {
        let dir = point - self.center();
        let start = self.center() + dir.normalize() * self.r;
        Line::new(start.x, start.y, point.x, point.y)
    }
    pub fn line_between(&self, other: Self) -> Line {
        let dir = other.center() - self.center();
        let start = self.center() + dir.normalize() * self.r;
        let end = other.center() - dir.normal() * other.r;
        Line::new(start.x, start.y, end.x, end.y)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Line {
    pub x0: f32,
    pub y0: f32,
    pub x1: f32,
    pub y1: f32,
}

impl Line {
    pub fn new(x0: f32, y0: f32, x1: f32, y1: f32) -> Self {
        Self { x0, y0, x1, y1 }
    }
    pub fn start(&self) -> Vec2 {
        Vec2::new(self.x0, self.y0)
    }
    pub fn end(&self) -> Vec2 {
        Vec2::new(self.x1, self.y1)
    }
    pub fn t(&self, t: f32) -> Vec2 {
        (self.end() - self.start()) * t + self.start()
    }
    pub fn center_rect(&self, width: f32, height: f32) -> Rect {
        Rect::new(
            self.t(0.5).x - width * 0.5,
            self.t(0.5).y - height * 0.5,
            self.t(0.5).x + width * 0.5,
            self.t(0.5).y + height * 0.5,
        )
    }
    pub fn bounds(&self) -> Rect {
        Rect::new(self.x0, self.y0, self.x1, self.y1)
    }
    pub fn len(&self) -> f32 {
        (self.end() - self.start()).len()
    }
}
