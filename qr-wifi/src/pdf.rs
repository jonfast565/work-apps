//use ::image::png::PngDecoder;
use image_crate::codecs::png::PngDecoder;
use printpdf::*;

use std::fs::File;
use std::io::{BufWriter, Cursor, Read};

use crate::args::CommandLineArgs;

pub fn build_pdf(args: &CommandLineArgs, qr_code_filename: &str) {
    let (doc, page1, layer1) = PdfDocument::new(
        format!("QR Code ({})", args.ssid),
        Mm(297.0),
        Mm(210.0),
        "Layer 1",
    );

    let current_layer = doc.get_page(page1).get_layer(layer1);
    let title_text = format!("Wireless Access");
    let ssid_text = format!("SSID: {}", args.ssid);
    let font = doc.add_builtin_font(BuiltinFont::Helvetica).unwrap();

    current_layer.use_text(title_text.clone(), 48.0, Mm(100.0), Mm(100.0), &font);
    current_layer.use_text(ssid_text.clone(), 36.0, Mm(100.0), Mm(200.0), &font);

    let mut file = File::open(qr_code_filename).unwrap();
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes).unwrap();
    let mut reader = Cursor::new(&mut bytes);
    let decoder = PngDecoder::new(&mut reader).unwrap();
    let img = Image::try_from(decoder).unwrap();

    img.add_to_layer(current_layer.clone(), ImageTransform::default());

    doc.save(&mut BufWriter::new(
        File::create(format!("./results/{}.pdf", args.ssid)).unwrap(),
    ))
    .unwrap();
}
