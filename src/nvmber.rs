use std::{
    fmt::{Debug, Display},
    ops::{Add, Sub},
    str::FromStr,
};

use thiserror::Error as ThisError;

#[derive(ThisError, Debug)]
pub enum Error {
    #[error("nvmber too large")]
    NvmberTooLarge(String),

    #[error("malformed")]
    Malformed(String),
}

#[derive(Clone)]
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

fn roman_to_integer(c: &char) -> Result<u64, Error> {
    let int = match c {
        '0' => 0,
        'I' => 1,
        'V' => 5,
        'X' => 10,
        'L' => 50,
        'C' => 100,
        'D' => 500,
        _ => return Err(Error::NvmberTooLarge(format!("{c}"))),
    };

    Ok(int)
}

impl Nvmber {
    pub fn from<S: AsRef<str>>(string: S) -> Result<Self, Error> {
        Nvmber::from_str(string.as_ref())
    }

    #[cfg(test)]
    pub fn get_integer(&self) -> u64 {
        self.integer
    }
}

impl FromStr for Nvmber {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut integer = 0;
        let mut prev_char: Option<char> = None;
        let mut repeats = 0;
        let mut prev_modifier: Option<char> = None;

        s.chars().try_for_each(|c| -> Result<_, Error> {
            let mut delta = roman_to_integer(&c)?;

            if let Some(prev_char) = prev_char {
                // If previous char is a smaller value, this means we have a modifier.
                let prev = roman_to_integer(&prev_char)?;
                if prev < delta {
                    if repeats > 0 {
                        return Err(Error::Malformed(format!(
                            "Roman {prev_char} used as modifier to {c}, but appears more than once"
                        )));
                    }

                    let allowed_modifiers = [
                        ('V', 'I'),
                        ('X', 'I'),
                        ('L', 'X'),
                        ('C', 'X'),
                        ('D', 'C'),
                        ('M', 'C'),
                    ];
                    if !allowed_modifiers.contains(&(c, prev_char)) {
                        return Err(Error::Malformed(format!(
                            "Roman {c} does not allow {prev_char} as a modifier"
                        )));
                    }

                    prev_modifier.replace(prev_char);

                    delta -= prev * 2; // * 2 because we added it on a previous loop
                }

                // Check for disallowed repeats.
                if c == prev_char {
                    repeats += 1;
                } else {
                    repeats = 0;
                }

                if repeats == 1 && ['V', 'L', 'D'].contains(&c) {
                    return Err(Error::Malformed(format!("{} appeared twice", c)));
                } else if repeats > 2 {
                    return Err(Error::Malformed(format!(
                        "{} appeared over 3 times in a row",
                        c
                    )));
                }

                // Check for trying to increase number that had a modifier.
                if let Some(prev_modifier) = prev_modifier {
                    if prev_modifier == c {
                        return Err(Error::Malformed(format!("Trying to increase {prev_char} with {c}, but had one before as modifier")));
                    }
                }
            }

            integer += delta;

            if integer > 3999 {
                return Err(Error::NvmberTooLarge(format!("Nvmber got too large while parsing {}", s)));
            }

            prev_char.replace(c.clone());

            Ok(())
        })?;

        Ok(Nvmber {
            chars: s.to_owned(),
            integer,
        })
    }
}

#[inline]
fn clamp_and_convert(v: u64) -> Nvmber {
    let mut v = v;

    if v > 3999 {
        v = 3999
    }

    v.to_nvmber()
        .expect("This should never happen, overflow on arithmetic?")
}

impl Add<&Nvmber> for &Nvmber {
    type Output = Nvmber;

    fn add(self, other: &Nvmber) -> Self::Output {
        clamp_and_convert(self.integer + other.integer)
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
        clamp_and_convert(self.integer - other.integer)
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

impl Eq for Nvmber {}

impl PartialEq for Nvmber {
    fn eq(&self, other: &Self) -> bool {
        self.integer == other.integer
    }
}

impl Ord for Nvmber {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.integer.cmp(&other.integer)
    }
}

impl PartialOrd for Nvmber {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub trait ToNvmber {
    fn to_nvmber(&self) -> Result<Nvmber, Error>;
}

impl ToNvmber for u64 {
    fn to_nvmber(&self) -> Result<Nvmber, Error> {
        let integer = *self;

        if integer > 3999 {
            return Err(Error::NvmberTooLarge(format!(
                "Nvmber can only go up to 3,999, but {} was attempted",
                *self
            )));
        }

        let intstr = integer.to_string();
        let mut chars = String::new();

        integer.to_string().chars().enumerate().for_each(|(i, c)| {
            let remaining = intstr.len() - i - 1;
            let (mono_char, mid_char, top_char) = match remaining {
                0 => ('I', 'V', 'X'),
                1 => ('X', 'L', 'C'),
                2 => ('C', 'D', 'M'),
                3 => ('M', '0', '0'),
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

        Ok(Nvmber { chars, integer })
    }
}

// STOLEN^WHeavily inspired on roman-numerals-rs's ToRoman
// https://github.com/ferdinandkeller/roman-numerals-rs/blob/master/src/to_roman.rs
macro_rules! impl_to_nvmber {
    ($t: tt) => {
        impl ToNvmber for $t {
            fn to_nvmber(&self) -> Result<Nvmber, Error> {
                (*self as u64).to_nvmber()
            }
        }
    };
}

macro_rules! impl_to_nvmber_for_ints {
    ($($t: ty),+) => {
        $(impl_to_nvmber!($t);)+
    };
}

impl_to_nvmber_for_ints![u8, u16, u32, i8, i16, i32, i64, usize];

#[cfg(test)]
mod test {
    use crate::nvmber::ToNvmber;

    use super::{Error, Nvmber};

    use rand::seq::SliceRandom;

    fn parse_nvmber(x: &str) -> Nvmber {
        Nvmber::from(x).unwrap_or_else(|_| panic!("Failed to parse nvmber {}", x))
    }

    #[test]
    fn test_nvmbers() {
        [
            ("I", 1),
            ("III", 3),
            ("IV", 4),
            ("V", 5),
            ("X", 10),
            ("XL", 40),
            ("L", 50),
            ("XC", 90),
            ("C", 100),
            ("CD", 400),
            ("D", 500),
            ("DCLXXVIII", 678),
        ]
        .iter()
        .for_each(|x| {
            let r = parse_nvmber(x.0);
            assert_eq!(x.1, r.integer);
        })
    }

    #[test]
    fn test_ordering() {
        let nvmbers: Vec<Nvmber> = ["I", "III", "IV", "V", "XV", "XVII", "L", "C", "CD", "D"]
            .iter()
            .map(|x| parse_nvmber(x))
            .collect();

        // Scramble the nvmbers, and make sure they have indeed been moved around.
        let mut scrambled = nvmbers.clone();
        scrambled.shuffle(&mut rand::thread_rng());

        assert_ne!(nvmbers, scrambled);

        // Now sort and check we get the expected ordering.
        let mut sorted = scrambled;
        sorted.sort();

        assert_eq!(nvmbers, sorted);
    }

    #[test]
    fn test_malformed() {
        ["IIII", "VV", "VX", "IL", "XIIV", "XD", "CDC"]
            .iter()
            .for_each(|s| {
                if !matches!(Nvmber::from(s), Err(Error::Malformed(..))) {
                    panic!("Expected error for malformed string '{}', but it parsed", s)
                };
            });
    }

    #[test]
    fn test_nvmber_too_large() {
        assert!(3999.to_nvmber().is_ok());

        [4000, 10000].iter().for_each(|n| {
            if !matches!(n.to_nvmber(), Err(Error::NvmberTooLarge(..))) {
                panic!("Expected error for {} being too large, but got OK", n)
            }
        });

        // FIXME: this should actually be a parse error.
        if !matches!(Nvmber::from("MMMCMXCXXII"), Err(Error::NvmberTooLarge(..))) {
            panic!("Expected an error for too large a string");
        }
    }
}
