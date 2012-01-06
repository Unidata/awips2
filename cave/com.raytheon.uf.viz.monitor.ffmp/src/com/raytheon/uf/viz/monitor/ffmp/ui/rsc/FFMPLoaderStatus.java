package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPDataLoader.LOADER_TYPE;

public class FFMPLoaderStatus {

    private LOADER_TYPE loaderType = null;

    private String message = null;

    private boolean isDone = false;

    public FFMPLoaderStatus(LOADER_TYPE loaderType, String message,
            boolean isDone) {
        this.loaderType = loaderType;
        this.message = message;
        this.isDone = isDone;
    }

    public LOADER_TYPE getLoaderType() {
        return loaderType;
    }

    public void setLoaderName(LOADER_TYPE loaderType) {
        this.loaderType = loaderType;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public boolean isDone() {
        return isDone;
    }

    public void isDone(boolean isDone) {
        this.isDone = isDone;
    }

}
