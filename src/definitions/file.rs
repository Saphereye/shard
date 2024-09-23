use std::fmt::{self, Display, Formatter};

#[rustfmt::skip]
pub enum File {
    A = 0, B, C, D, E, F, G, H, None,
}

impl Display for File {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            File::A => write!(f, "a"),
            File::B => write!(f, "b"),
            File::C => write!(f, "c"),
            File::D => write!(f, "d"),
            File::E => write!(f, "e"),
            File::F => write!(f, "f"),
            File::G => write!(f, "g"),
            File::H => write!(f, "h"),
            File::None => write!(f, "None"),
        }
    }
}
