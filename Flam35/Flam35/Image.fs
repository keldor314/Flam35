module Image

open System.IO
open System.Windows.Media
open System.Windows.Media.Imaging


let writeImage (buffer: float32 []) width height useAlpha file =
    let format = if useAlpha then PixelFormats.Rgba128Float else PixelFormats.Rgb128Float
    let source = 
        BitmapSource.Create ( width, height,
                              96., 96.,
                              format,
                              null,
                              buffer,
                              16*width )
    let encoder = new WmpBitmapEncoder ()
    let output = new FileStream(file, FileMode.OpenOrCreate)
    encoder.Frames.Add <| BitmapFrame.Create source
    encoder.Save output
    ()