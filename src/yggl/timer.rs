use std::str::FromStr;

pub enum TimerType {
    Periodic,
}

pub enum TimeUnit {
    Hour,
    Minute,
    Second,
    Milli,
}

impl TimeUnit {
    pub fn scale(&self, value: u64) -> u64 {
        match &self {
            TimeUnit::Hour => value * 60 * 60 * 1000,
            TimeUnit::Minute => value * 60 * 1000,
            TimeUnit::Second => value * 1000,
            TimeUnit::Milli => value,
        }
    }
}


impl FromStr for TimeUnit {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "h" => Ok(TimeUnit::Hour),
            "m" => Ok(TimeUnit::Minute),
            "s" => Ok(TimeUnit::Second),
            "ms" => Ok(TimeUnit::Milli),
            _ => Err(())
        }
    }
}