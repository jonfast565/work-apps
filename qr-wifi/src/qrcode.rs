use image::Luma;
use qrcode::QrCode;

pub fn get_qr_code(to_be_encoded: &str, width: u32, height: u32) -> image::ImageBuffer<Luma<u8>, Vec<u8>> {
    let code = QrCode::new(to_be_encoded).unwrap();
    let image = code.render::<Luma<u8>>()
    .min_dimensions(width, height)
    .max_dimensions(width, height)
    .build();
    image
}