package com.raytheon.uf.viz.core.drawables;

import org.eclipse.swt.graphics.RGB;

public class SingleColorImage implements IImage {

    private IImage image = null;

    private RGB color = null;

    public IImage getWrappedImage() {
        return image;
    }

    public void setWrappedImage(IImage image) {
        this.image = image;
    }

    public void setColor(RGB color) {
        this.color = color;
    }

    public RGB getColor() {
        return color;
    }

    public SingleColorImage(IImage image) {
        this.image = image;
    }

    @Override
    public Status getStatus() {
        return image.getStatus();
    }

    @Override
    public void setInterpolated(boolean isInterpolated) {
        image.setInterpolated(isInterpolated);
    }

    @Override
    public void dispose() {
        image.dispose();
    }

    @Override
    public int getWidth() {
        return image.getWidth();
    }

    @Override
    public int getHeight() {
        return image.getHeight();
    }

    @Override
    public void setBrightness(float brightness) {
        image.setBrightness(brightness);
    }

    @Override
    public void setContrast(float contrast) {
        image.setContrast(contrast);
    }

}
