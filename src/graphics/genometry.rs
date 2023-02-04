use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Clone, Copy)]
pub struct Vec2 {
    pub x: f32,
    pub y: f32,
}

impl Vec2 {
    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    pub fn dot(&self, other: &Self) -> f32 {
        self.x * other.x + self.y * other.y
    }
    pub fn len_squared(&self) -> f32 {
        self.dot(self)
    }
    pub fn len(&self) -> f32 {
        self.len_squared().sqrt()
    }
    pub fn normalize(&self) -> Self {
        self / self.len()
    }
    pub fn normal(&self) -> Self {
        Self {
            x: self.y,
            y: self.x,
        }
    }
    pub fn t(&self, t: f32) -> Self {
        Self {
            x: self.x * t,
            y: self.y * t,
        }
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

impl Add<Vec2> for &Vec2 {
    type Output = Vec2;

    fn add(self, rhs: Vec2) -> Self::Output {
        Vec2::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl Sub<Vec2> for &Vec2 {
    type Output = Vec2;

    fn sub(self, rhs: Vec2) -> Self::Output {
        Vec2::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl Mul<f32> for &Vec2 {
    type Output = Vec2;

    fn mul(self, rhs: f32) -> Self::Output {
        Vec2::new(self.x * rhs, self.y * rhs)
    }
}

impl Div<f32> for &Vec2 {
    type Output = Vec2;

    fn div(self, rhs: f32) -> Self::Output {
        Vec2::new(self.x / rhs, self.y / rhs)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Rect {
    pub x0: f32,
    pub y0: f32,
    pub x1: f32,
    pub y1: f32,
}

impl Rect {
    pub fn new(x0: f32, y0: f32, x1: f32, y1: f32) -> Self {
        Self {
            x0: f32::min(x0, x1),
            y0: f32::min(y0, y1),
            x1: f32::max(x0, x1),
            y1: f32::max(y0, y1),
        }
    }
    pub fn width(&self) -> f32 {
        self.x1 - self.x0
    }
    pub fn height(&self) -> f32 {
        self.y1 - self.y0
    }
    pub fn uv(&self, u: f32, v: f32) -> Vec2 {
        let x = Vec2::new(self.width(), 0.0);
        let y = Vec2::new(0.0, self.height());

        Vec2::new(self.x0, self.y0) + x * u + y * v
    }
    pub fn center_rect(&self, width: f32, height: f32) -> Rect {
        let x = if width >= self.width() {
            0.0
        } else {
            self.width() - width
        } + self.uv(0.0, 0.0).x;
        let y = if height >= self.height() {
            0.0
        } else {
            self.height() - height
        } + self.uv(0.0, 0.0).y;

        Rect::new(x, y, x + width, y + height)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RotatedRect {
    pub center: Vec2,
    pub size: (f32, f32),
    pub x_facing: Vec2,
}

impl RotatedRect {
    pub fn new(center: Vec2, size: (f32, f32), x_facing: Vec2) -> Self {
        Self {
            center,
            size,
            x_facing: x_facing.normalize(),
        }
    }

    pub fn width(&self) -> f32 {
        self.size.0
    }
    pub fn height(&self) -> f32 {
        self.size.1
    }

    pub fn uv(&self, u: f32, v: f32) -> Vec2 {
        let left = self.x_facing * self.width();
        let up = self.x_facing.normal() * self.height();
        left * (u - 0.5) + up * (v - 0.5) + self.center
    }
}

impl From<Rect> for RotatedRect {
    fn from(r: Rect) -> Self {
        Self::new(r.uv(0.5, 0.5), (r.width(), r.height()), Vec2::new(1.0, 0.0))
    }
}

impl<'a> From<&'a Rect> for RotatedRect {
    fn from(r: &'a Rect) -> Self {
        Self::new(r.uv(0.5, 0.5), (r.width(), r.height()), Vec2::new(1.0, 0.0))
    }
}
