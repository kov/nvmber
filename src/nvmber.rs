use std::{
    fmt::{Debug, Display},
    ops::{Add, ControlFlow, Sub},
};

pub struct Nvmber {
    chars: String,
    integer: u64,
}

impl Display for Nvmber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.chars, f)
    }
}

impl Debug for Nvmber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.chars, self.integer)
    }
}

impl Nvmber {
    pub fn from<S: AsRef<str>>(string: S) -> Option<Self> {
        Nvmber::parse(string.as_ref())
    }

    pub fn from_integer(integer: u64) -> Self {
        let intstr = integer.to_string();
        let mut chars = String::new();

        integer.to_string().chars().enumerate().for_each(|(i, c)| {
            let remaining = intstr.len() - i - 1;
            let (mono_char, mid_char, top_char) = match remaining {
                0 => ('I', 'V', 'X'),
                1 => ('X', 'L', 'C'),
                2 => ('C', 'D', 'M'),
                _ => unimplemented!(),
            };
            let to_add = match c {
                n if n == '9' => format!("{}{}", mono_char, top_char),
                n if n == '8' => format!("{}{}{}{}", mid_char, mono_char, mono_char, mono_char),
                n if n == '7' => format!("{}{}{}", mid_char, mono_char, mono_char),
                n if n == '6' => format!("{}{}", mid_char, mono_char),
                n if n == '5' => format!("{}", mid_char),
                n if n == '4' => format!("{}{}", mono_char, mid_char),
                n if n == '3' => format!("{}{}{}", mono_char, mono_char, mono_char),
                n if n == '2' => format!("{}{}", mono_char, mono_char),
                n if n == '1' => format!("{}", mono_char),
                n if n == '0' => format!(""),
                _ => unimplemented!(),
            };
            chars += &to_add;
        });

        Nvmber { chars, integer }
    }

    pub fn parse<S: AsRef<str>>(string: S) -> Option<Self> {
        let mut integer = 0;
        let mut prev_char = '0';
        string.as_ref().chars().try_for_each(|c| {
            let delta = match c {
                'I' => 1,
                'V' if prev_char == 'I' => 4 - 1,
                'V' => 5,
                'X' if prev_char == 'I' => 9 - 1,
                'X' => 10,
                'L' if prev_char == 'X' => 40 - 10,
                'L' => 50,
                'C' if prev_char == 'L' => 90 - 10,
                'C' => 100,
                'D' if prev_char == 'C' => 400 - 100,
                'D' => 500,
                _ => return ControlFlow::Break(c),
            };

            integer += delta;

            prev_char = c.clone();

            ControlFlow::Continue(())
        });
        Some(Nvmber {
            chars: string.as_ref().to_owned(),
            integer,
        })
    }
}

impl Add<&Nvmber> for &Nvmber {
    type Output = Nvmber;

    fn add(self, other: &Nvmber) -> Self::Output {
        Nvmber::from_integer(self.integer + other.integer)
    }
}

impl Add<Nvmber> for Nvmber {
    type Output = Nvmber;

    fn add(self, other: Nvmber) -> Self::Output {
        &self + &other
    }
}

impl Add<Nvmber> for &Nvmber {
    type Output = Nvmber;

    fn add(self, other: Nvmber) -> Self::Output {
        self + &other
    }
}

impl Add<&Nvmber> for Nvmber {
    type Output = Nvmber;

    fn add(self, other: &Nvmber) -> Self::Output {
        &self + other
    }
}

impl Sub<&Nvmber> for &Nvmber {
    type Output = Nvmber;

    fn sub(self, other: &Nvmber) -> Self::Output {
        Nvmber::from_integer(self.integer - other.integer)
    }
}

impl Sub<Nvmber> for Nvmber {
    type Output = Nvmber;

    fn sub(self, other: Nvmber) -> Self::Output {
        &self - &other
    }
}

impl Sub<Nvmber> for &Nvmber {
    type Output = Nvmber;

    fn sub(self, other: Nvmber) -> Self::Output {
        self - &other
    }
}

impl Sub<&Nvmber> for Nvmber {
    type Output = Nvmber;

    fn sub(self, other: &Nvmber) -> Self::Output {
        &self - other
    }
}
