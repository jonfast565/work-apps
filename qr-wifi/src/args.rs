use clap::Parser;

#[derive(Debug, PartialEq, Clone, clap::ValueEnum)]
pub enum WifiSecurityType {
    Wep,
    Wpa,
    NoPassword,
}


#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct CommandLineArgs {
    #[clap(short = 's', long, value_parser, required = true)]
    pub ssid: String,

    #[clap(short, long, value_parser, required = true)]
    pub password: String,

    #[clap(short = 'c', long, required = true, value_enum)]
    pub security: WifiSecurityType,
}

impl CommandLineArgs {
    pub fn parse_args() -> CommandLineArgs {
        CommandLineArgs::parse()
    }
    
    pub fn show_args(&self) {
        println!("SSID: {}", self.ssid);
        println!("Password: {}", self.password.chars().map(|_| '*').collect::<String>());
        println!("Security: {:?}", self.security);
    }

    pub fn get_wifi_security_string(&self) -> &str {
        match self.security {
            WifiSecurityType::Wep => "WEP",
            WifiSecurityType::Wpa => "WPA",
            WifiSecurityType::NoPassword => "",
        }
    }
    
    pub fn get_formatted_wifi_string(&self) -> String {
        let wifi = format!("WIFI:S:{};T:{};P:{};;", self.ssid, self.get_wifi_security_string(), self.password);
        wifi
    }
}
