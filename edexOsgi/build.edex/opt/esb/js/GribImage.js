/**
 * Creates an image from a GRIB record. Maps a selected color map to the
 * image, rescales the image and converts it to a specified output format.
 *
 * This file may be imported into a JS uEngine script needing to create
 * an image from a GRIB record.
 *
 * Usage:
 *   include("GribImage.js");
 *   var iMaker = new GribImage("grib");
 *   iMaker.setColormap("GribRGB");
 *   iMaker.setFormat("png");
 *   iMaker.setGrid(grid);  // 'grid' is set to a previously ingested grib data
 *   iMaker.setGeom(geom);  // 'geom' is set to the geometry from the grib
 *   iMaker.setCrs(crs);    // 'crs' is set to the CRS from the grib
 *   var image = iMaker.execute();
 */

/**
 * Class constructor.
 *
 * @param plugin (String) identifies the data-type plugin - normally "grib"
 */
function GribImage(plugin) {
  /* names of things */
  this.plugin = plugin;
  /* grid attributes */
  this.grid = null;
  this.geom = null;
  this.crs = null;
  /* image attributes */
  this.colormap = "GribRGB";
  this.format = "png";
  this.scaleFactor = 1;
  this.reproject = false;
}
/**
 * Main action method. Performs the the GRIB to an image object.
 *
 * @return (byte[]) raster containing the image
 *         (String) XML null response string on failure
 */
function _execute() {
  var gribMap = new GribMap(this.grib, this.colormap, this.grid, this.geom);
  gribMap.setScaleFactor(this.scaleFactor);
  var imageData = gribMap.execute();
  this.geom = gribMap.getGridGeometry();
  var colorMap = new ColorMapImage(this.colormap, imageData, this.geom);
  var imageOut = null;
  if(this.reproject){
    var reproject = new ReprojectImage(colorMap.execute(), this.geom, this.crs);
    var reprojectedImage = reproject.execute();
    imageOut = new ImageOut(reprojectedImage, this.format, reproject.getGridGeometry());
  }
  else
  {
    imageOut = new ImageOut(colorMap.execute(), this.format,this.geom);
  }
  return imageOut.execute();
}
/* setters */
function _setGrid(grid) {
  this.grid = grid;
}
function _setGeom(geom) {
  this.geom = geom;
}
function _setCrs(crs) {
  this.crs = crs;
}
function _setColormap(colormap) {
  this.colormap = colormap;
}
function _setFormat(format) {
  this.format = format;
}
function _setScaleFactor(scale) {
  this.scaleFactor = scale;
}
function _setReproject(reproject) {
  this.reproject = reproject;
}
/* getters */
function _getGeom() {
  return this.geom;
}
function _getFormat() {
  return this.format;
}

/* map the functions to the class prototype */
GribImage.prototype.execute = _execute;
GribImage.prototype.setGrid = _setGrid;
GribImage.prototype.setGeom = _setGeom;
GribImage.prototype.getGeom = _getGeom;
GribImage.prototype.setCrs = _setCrs;
GribImage.prototype.setColormap = _setColormap;
GribImage.prototype.setFormat = _setFormat;
GribImage.prototype.getFormat = _getFormat;
GribImage.prototype.setScaleFactor = _setScaleFactor;
GribImage.prototype.setReproject = _setReproject;
