mod args;
mod qrcode;
mod pdf;

use crate::args::CommandLineArgs;

fn main() {
    let args = CommandLineArgs::parse_args();
    args.show_args();
    std::fs::create_dir_all("./results");
    let image = qrcode::get_qr_code(args.get_formatted_wifi_string().as_str(), 800, 800);
    let result_filename = format!("./results/{}.png", args.ssid);
    image.save(result_filename.clone()).unwrap();
    pdf::build_pdf(&args, result_filename.as_str());
}
