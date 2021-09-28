#'@name LARGB
#'
#'@title Leaf Area Determination from Visual Image
#'
#' @description High-throughput plant phenotyping using image analysis is the key area in the domain of plant phenotyping. For determining the leaf area, the RGB image is converted into the grayscale image by simply averaging the Red(R), Green (G) and Blue (B) pixel values. Grayscale image is then converted into a binary image using Otsu’s thresholding method Otsu, N. (1979) <doi: 10.1109/TSMC.1979.4310076> to separate plant area from the background (image segmentation). The segmentation process was accomplished by selecting the pixels with values over the threshold value belonging to the plant region and other pixels to the background region. The resulting binary image consists of white and black pixels representing the plant and background regions, respectively. Finally, the number of pixels inside the plant region was counted and converted to square centimetres (cm2) using the reference object (any object whose actual area is known previously) to get the projected leaf area.
#'
#' @param img_path character string containing file path of the visual(RGB) image
#' @param  ref_area a numeric value containing known pixel area (in cm square) of the reference object
#'
#' @return Pixel area along with the leaf area in cm square
#'
#' @import  "imager"
#'
#' @import  "dplyr"
#'
#' @export
#'
#' @examples
#' fpath= system.file('extdata/test1.jpg', package = 'LARGB')
#' LARGB(fpath, 0.025)
#'
#' @references
#' Patil, S. B., & Bodhe, S. K. (2011). Betel leaf area measurement using image processing. \emph{International Journal on Computer Science and Engineering}, 3(7), 2656-2660.
#' \cr Misra, T., Marwaha, S., Arora, A., Ray, M.,Kumar, S., Kumar, S. (2021). Leaf area assessment using image processing and support vector regression in rice. \emph{Indian Journal of Agricultural Sciences}, 91 (3), 388–92.
#' \cr Xu, X., Xu, S., Jin, L, and Song, E. (2011). Characteristic analysis of Otsu threshold and its applications. \emph{Pattern Recognition Letters}, 32(7), 956–61.
#'
#' @keywords LeafArea ImageAnalysis RGB
#'
#' @export
#'
LARGB <- function(img_path, ref_area){
  rgb= load.image(img_path)
  gray_of_Sub_of_rgb_bg=grayscale(rgb)
  th_of_Sub_of_rgb_bg=threshold(gray_of_Sub_of_rgb_bg, thr = "auto")
  img=as.cimg(th_of_Sub_of_rgb_bg)
  pixel_area=(height(img)*width(img))-sum(img)

  plot(rgb, main="RGB image")
  plot(gray_of_Sub_of_rgb_bg, main=" Grayscale image")
  plot(th_of_Sub_of_rgb_bg, main=" Binary image")
  return(c('pixel_area' = pixel_area, 'leaf area (cm square)' = pixel_area*ref_area))
}
