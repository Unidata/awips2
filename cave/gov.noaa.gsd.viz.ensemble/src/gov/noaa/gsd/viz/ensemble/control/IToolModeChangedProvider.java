package gov.noaa.gsd.viz.ensemble.control;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool.EnsembleToolMode;

public interface IToolModeChangedProvider {

    public void addToolModeChangedListener(IToolModeChangedListener listener);

    public void removeToolModeChangedListener(
            IToolModeChangedListener listener);

    public void notifyToolModeChanged(EnsembleToolMode toolmode);

}
