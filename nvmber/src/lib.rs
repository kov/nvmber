use std::{
    fmt::{Debug, Display},
    ops::{Add, Neg, Sub},
    str::FromStr,
};

use thiserror::Error as ThisError;

#[derive(ThisError, Debug, PartialEq)]
pub enum Error {
    #[error("nvmber too large")]
    NvmberTooLarge(String),

    #[error("malformed")]
    Malformed(String),
}

#[repr(i64)]
#[derive(Clone, Debug, PartialEq, PartialOrd)]
enum Numeri {
    I = 1,
    V = 5,
    X = 10,
    L = 50,
    C = 100,
    D = 500,
    M = 1000,
}

impl Display for Numeri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Numeri::I => 'I',
            Numeri::V => 'V',
            Numeri::X => 'X',
            Numeri::L => 'L',
            Numeri::C => 'C',
            Numeri::D => 'D',
            Numeri::M => 'M',
        };
        write!(f, "{c}")
    }
}

impl Numeri {
    fn is_beyond_max_repeat(&self, repeated: usize) -> bool {
        // No element can go beyond 3, so 2 repeats is the max allowed.
        if repeated > 2 {
            return true;
        }

        match self {
            Numeri::V | Numeri::L | Numeri::D => repeated > 0,
            _ => false,
        }
    }

    fn is_allowed_after(&self, other: &Numeri, other_repeat: usize) -> bool {
        match self {
            Numeri::I => true,
            Numeri::V => match other {
                // V can come after anything, but if it comes after I, that
                // is being used to modify it and can only be there once.
                Numeri::I => other_repeat == 0,
                _ => true,
            },
            Numeri::X => match other {
                Numeri::I => other_repeat == 0,
                Numeri::V => false,
                _ => true,
            },
            Numeri::L => match other {
                Numeri::I | Numeri::V => false,
                Numeri::X => other_repeat == 0,
                _ => true,
            },
            Numeri::C => match other {
                Numeri::I | Numeri::V | Numeri::L => false,
                Numeri::X => other_repeat == 0,
                _ => true,
            },
            Numeri::D => match other {
                Numeri::I | Numeri::V | Numeri::X | Numeri::L => false,
                Numeri::C => other_repeat == 0,
                _ => true,
            },
            Numeri::M => match other {
                Numeri::I | Numeri::V | Numeri::L | Numeri::D => false,
                Numeri::C => other_repeat == 0,
                _ => true,
            },
        }
    }
}

struct Parser {
    previous: Option<Numeri>,
    previous_repeat: usize,
    last: Option<Numeri>,
    current_repeat: usize,
    remaining: Vec<Numeri>, // Reversed.
}

impl Parser {
    fn new(s: &str) -> Result<Self, Error> {
        let numeri = s
            .chars()
            .rev()
            .map(|c| {
                let n = match c {
                    'I' => Numeri::I,
                    'V' => Numeri::V,
                    'X' => Numeri::X,
                    'L' => Numeri::L,
                    'C' => Numeri::C,
                    'D' => Numeri::D,
                    'M' => Numeri::M,
                    _ => return Err(Error::Malformed(format!("Unkwown element {c} in {s}"))),
                };
                Ok(n)
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Parser {
            previous: None,
            previous_repeat: 0,
            last: None,
            current_repeat: 0,
            remaining: numeri,
        })
    }

    fn step(&mut self) -> Result<i64, Error> {
        if self.remaining.is_empty() {
            return Ok(0);
        }

        // SAFETY: the check above means we will always have an item to pop.
        let c = self.remaining.pop().unwrap();

        if let Some(previous) = &self.previous {
            if !c.is_allowed_after(previous, self.previous_repeat) {
                return Err(Error::Malformed(format!(
                    "Found {c} after {previous}, which is not allowed"
                )));
            }
        }

        // Update repeat tracking.
        if let Some(last) = &self.last {
            if *last == c {
                self.current_repeat += 1;
            }
        }

        if c.is_beyond_max_repeat(self.current_repeat) {
            return Err(Error::Malformed(format!(
                "Element {c} repeated more than expected"
            )));
        }

        if let Some(next) = self.remaining.last() {
            // Check for elements being used to both subtract and add.
            if let Some(previous) = &self.previous {
                // This is only a problem if the current element is the one being modified, not the modifier. We test that by checking that the next element is larger.
                if *previous == *next && *next < c {
                    return Err(Error::Malformed(format!(
                        "Found {previous} both subtracting and adding to {c}"
                    )));
                }

                // Protect against chains of subtractions that are supported on their own.
                // e.g. XCD, IXL.
                if *previous < c && *next > c {
                    return Err(Error::Malformed(format!(
                        "Two subtractions in a row {previous} to {c} and {c} to {next}"
                    )));
                }
            }
        }

        // The integer amount we need to add (or subtract, see below).
        let mut delta = c.clone() as i64;

        // Update tracking for previous if the next element is different, and reset repeat tracking.
        if let Some(next) = self.remaining.last() {
            if *next != c {
                self.previous.replace(c.clone());
                self.previous_repeat = self.current_repeat;
                self.current_repeat = 0;
            }

            // If the next element is larger than us, that means we are being used as a subtractor.
            if *next > c {
                delta = -delta;
            }
        }

        // Keep track of last element.
        self.last.replace(c.clone());

        Ok(delta)
    }

    fn parse(&mut self) -> Result<i64, Error> {
        let mut integer = 0;
        loop {
            let delta = self.step()?;

            if delta == 0 {
                return Ok(integer);
            }

            integer += delta;

            if integer > 3999 {
                return Err(Error::NvmberTooLarge(format!(
                    "Nvmber got too large (> 3,999) while parsing string"
                )));
            }
        }
    }
}

#[derive(Clone)]
pub struct Nvmber {
    chars: String,
    integer: i64,
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
    pub fn from<S: AsRef<str>>(string: S) -> Result<Self, Error> {
        Nvmber::from_str(string.as_ref())
    }

    pub fn get_integer(&self) -> i64 {
        self.integer
    }
}

impl FromStr for Nvmber {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let integer = Parser::new(s)?.parse()?;

        Ok(Nvmber {
            chars: s.to_owned(),
            integer,
        })
    }
}

#[inline]
fn clamp_and_convert(v: i64) -> Nvmber {
    let mut v = v;

    if v > 3999 {
        v = 3999
    } else if v < -3999 {
        v = -3999;
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

impl Neg for &Nvmber {
    type Output = Nvmber;

    fn neg(self) -> Self::Output {
        Nvmber {
            chars: self.chars.clone(),
            integer: -self.integer,
        }
    }
}

impl Neg for Nvmber {
    type Output = Nvmber;

    fn neg(self) -> Self::Output {
        (&self).neg()
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

impl ToNvmber for i64 {
    fn to_nvmber(&self) -> Result<Nvmber, Error> {
        let integer = *self;

        if integer > 3999 || integer < -3999 {
            return Err(Error::NvmberTooLarge(format!(
                "Nvmber can only go from -3,999 to 3,999, but {} was attempted",
                *self
            )));
        }

        let mut chars = String::new();

        if integer < 0 {
            chars += "-";
        }

        let intstr = integer.abs().to_string();
        intstr.chars().enumerate().for_each(|(i, c)| {
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
                (*self as i64).to_nvmber()
            }
        }
    };
}

macro_rules! impl_to_nvmber_for_ints {
    ($($t: ty),+) => {
        $(impl_to_nvmber!($t);)+
    };
}

impl_to_nvmber_for_ints![u8, u16, u32, u64, i8, i16, i32, usize];

#[cfg(test)]
mod test {
    use super::{Error, Nvmber, ToNvmber};

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
    fn test_from_str() {
        ["I", "XV", "MDC"]
            .iter()
            .for_each(|s| assert!(Nvmber::from(s).is_ok()));
    }

    #[test]
    fn test_malformed() {
        ["IIII", "VV", "VX", "IL", "XIIV", "XD", "CDC", "MMMCMXCXXII"]
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
    }

    #[test]
    fn test_arithmetic_overflow() {
        // Overflows, stays at the maximum.
        assert_eq!(
            3999.to_nvmber().unwrap() + 10.to_nvmber().unwrap(),
            3999.to_nvmber().unwrap()
        );

        assert_eq!(
            -3999.to_nvmber().unwrap() - 10.to_nvmber().unwrap(),
            -3999.to_nvmber().unwrap()
        );

        // No overflow.
        assert_eq!(
            3988.to_nvmber().unwrap() + 10.to_nvmber().unwrap(),
            3998.to_nvmber().unwrap()
        );
    }

    use super::Parser;

    #[test]
    fn test_parser() {
        [
            ("I", 1),
            ("XV", 15),
            ("CXC", 190),
            ("CCCXC", 390),
            ("MDC", 1600),
        ]
        .iter()
        .for_each(|(s, i)| {
            assert_eq!(
                Parser::new(s).expect("Failed initial parse").parse(),
                Ok(*i)
            )
        });

        [
            "IIII",
            "VV",
            "VX",
            "IL",
            "IIX",
            "IXL",
            "XIIV",
            "XCD",
            "XD",
            "CDC",
            "MMMCMXCXXII",
        ]
        .iter()
        .for_each(|s| {
            if !matches!(
                Parser::new(s).expect("Failed initial parse").parse(),
                Err(Error::Malformed(..))
            ) {
                panic!("Expected error for malformed string '{}', but it parsed", s)
            };
        });
    }
}
