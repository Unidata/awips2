The [python-awips](https://python-awips.readthedocs.io/en/latest/) package provides a data access framework for requesting grid and geometry datasets from an EDEX server.

!!! info "For a more detailed look at the python-awips package, refer to the [full documentation site](https://python-awips.readthedocs.io) which includes a number of [plotting examples for different data types](https://python-awips.readthedocs.io/en/latest/examples/index.html)."

---

## Install

    pip install python-awips

---

## Requirements

* Python 2.7+
* Numpy 1.7+
* Shapely 1.4+
* MetPy and enum34 to run Jupyter Notebook examples

---

## Example

The simplest example requesting the RAP40 surface temperature grid from a remote EDEX server, saved to 2-dimensional Numpy arrays named `data`, `lons`, and `lats`.

    from awips.dataaccess import DataAccessLayer
    DataAccessLayer.changeEDEXHost("edex-cloud.unidata.ucar.edu")
    request = DataAccessLayer.newDataRequest()
    dataTypes = DataAccessLayer.getSupportedDatatypes()
    request.setDatatype("grid")
    request.addLocationNames("RAP40")
    request.setParameters("T")
    request.setLevels("0.0SFC")
    cycles = DataAccessLayer.getAvailableTimes(request, True)
    times = DataAccessLayer.getAvailableTimes(request)
    response = DataAccessLayer.getGridData(request, times[-1])
    for grid in response:
        data = grid.getRawData()
        lons, lats = grid.getLatLonCoords()


