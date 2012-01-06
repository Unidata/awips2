package gov.noaa.nws.ncep.viz.ui.locator.resource;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;

public interface ILocator {
    public abstract INatlCntrsResourceData getResourceData();

    public abstract void resourceAttrsModified();

}
