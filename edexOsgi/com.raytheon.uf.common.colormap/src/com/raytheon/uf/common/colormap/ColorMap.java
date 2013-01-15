/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.colormap;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Implementation of a colormap
 *
 * <pre>
 *
 *  SOFTWARE HISTORY
 *
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Feb 5, 2007              chammack    Initial Creation.
 *  Jul 24, 2007             njensen     Moved to EDEX, added setters.
 *  Aug 20, 2008			 bclement	 Added JaXB annotations and color class
 *  Aug 20, 2008			 dglazesk	 Added some interface pieces to make this more
 *                                       usable
 *  Jan 10, 2013 15648       ryu         Added removeDuplicates() method.
 *
 * </pre>
 *
 * @author chammack
 * @version 1
 */
@XmlRootElement(name = "colorMap")
@XmlAccessorType(XmlAccessType.NONE)
public class ColorMap extends AbstractColorMap implements ISerializableObject {

    @XmlElements({ @XmlElement(name = "color", type = Color.class) })
    List<Color> colors;

    private String name;

    private Buffer glBuffer;

    private boolean changed = false;

    /**
     * Constructor used by JiBX
     */
    public ColorMap() {
        this.colors = new ArrayList<Color>();
    }

    public ColorMap(int size) {
        this.colors = new ArrayList<Color>(Arrays.asList(new Color[size]));
    }

    /**
     *
     *
     */
    public ColorMap(String name, float[] red, float[] green, float[] blue) {
        this.colors = new ArrayList<Color>();

        int arraySize = red.length;
        for (int i = 0; i < arraySize; ++i) {
            colors.add(new Color(red[i], green[i], blue[i]));
        }

        this.name = name;
    }

    /**
     * Creates a ColorMap using name and the list of colors in map.
     *
     * @param name
     *            Name for the new ColorMap
     * @param map
     *            ColorMap to base the colors off of
     */
    public ColorMap(String name, ColorMap map) {
        this.name = name;
        colors = new ArrayList<Color>();
        colors.addAll(map.getColors());
        this.changed = true;
    }

    /**
     * Do not instantiate directly, use GLTarget methods
     *
     * @param name
     * @param red
     * @param green
     * @param blue
     * @param alpha
     */
    public ColorMap(String name, float[] red, float[] green, float[] blue,
            float[] alpha) {
        this.colors = new ArrayList<Color>();

        int arraySize = red.length;
        for (int i = 0; i < arraySize; ++i) {
            colors.add(new Color(red[i], green[i], blue[i], alpha[i]));
        }

        this.name = name;
    }

    public ColorMap(String name, int numColors, float minWaveLength,
            float maxWaveLength, boolean reverse) {
        this.name = name;
        spread(numColors, minWaveLength, maxWaveLength, reverse);
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.colormap.IColorMap#getBlue()
     */
    public float[] getBlue() {
        int colorNum = colors.size();
        float[] blues = new float[colorNum];
        for (int i = 0; i < colorNum; ++i) {
            blues[i] = colors.get(i).getBlue();
        }
        return blues;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.colormap.IColorMap#getGreen()
     */
    public float[] getGreen() {
        int colorNum = colors.size();
        float[] greens = new float[colorNum];
        for (int i = 0; i < colorNum; ++i) {
            greens[i] = colors.get(i).getGreen();
        }
        return greens;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.colormap.IColorMap#getRed()
     */
    public float[] getRed() {
        int colorNum = colors.size();
        float[] reds = new float[colorNum];
        for (int i = 0; i < colorNum; ++i) {
            reds[i] = colors.get(i).getRed();
        }
        return reds;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.colormap.IColorMap#getSize()
     */
    public int getSize() {
        return colors.size();
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.colormap.IColorMap#getAlpha()
     */
    public float[] getAlpha() {
        int colorNum = colors.size();
        float[] alphas = new float[colorNum];
        for (int i = 0; i < colorNum; ++i) {
            alphas[i] = colors.get(i).getAlpha();
        }
        return alphas;
    }

    /**
     * Return the buffer representation of the colormap
     *
     * @return the colorMap buffer
     */
    public synchronized Buffer getColorMap() {
        if (glBuffer == null || changed) {
            ByteBuffer byteBuffer = ByteBuffer.allocateDirect(4 * 4 * colors
                    .size());
            byteBuffer.order(ByteOrder.nativeOrder());
            byteBuffer.rewind();
            FloatBuffer floatBuffer = byteBuffer.asFloatBuffer();
            floatBuffer.rewind();
            for (int i = 0; i < colors.size(); i++) {
                Color c = colors.get(i);
                floatBuffer.put(c.getRed());
                floatBuffer.put(c.getGreen());
                floatBuffer.put(c.getBlue());
                floatBuffer.put(c.getAlpha());
            }

            floatBuffer.rewind();

            glBuffer = floatBuffer;
        }

        return glBuffer;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.edex.colormap.IColorMap#getName()
     */
    public String getName() {
        return name;
    }

    public List<Color> getColors() {
        return colors;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "COLORMAP { " + getName() + " }";
    }

    public boolean isChanged() {
        return changed;
    }

    public void setAlpha(float[] anAlpha) {
        int size = anAlpha.length;
        for (int i = 0; i < size; ++i) {
            if (i > colors.size()) {
                colors.add(new Color(0, 0, 0));
            }

            colors.get(i).setAlpha(anAlpha[i]);
        }
        changed = true;
    }

    public void setBlue(float[] aBlue) {
        int size = aBlue.length;
        for (int i = 0; i < size; ++i) {
            if (i > colors.size()) {
                colors.add(new Color(0, 0, 0));
            }

            colors.get(i).setBlue(aBlue[i]);
        }
        changed = true;
    }

    public void setGreen(float[] aGreen) {
        int size = aGreen.length;
        for (int i = 0; i < size; ++i) {
            if (i > colors.size()) {
                colors.add(new Color(0, 0, 0));
            }

            colors.get(i).setGreen(aGreen[i]);
        }
        changed = true;
    }

    public void setRed(float[] aRed) {
        int size = aRed.length;
        for (int i = 0; i < size; ++i) {
            if (i > colors.size()) {
                colors.add(new Color(0, 0, 0));
            }

            colors.get(i).setRed(aRed[i]);
        }
        changed = true;
    }

    public void setColor(int index, Color color) {
        this.colors.set(index, color);
    }

    public void setChanged(boolean aChanged) {
        changed = aChanged;
    }

    public void setName(String aName) {
        name = aName;
        changed = true;
    }

    @Override
    public IColorMap clone() {
        return new ColorMap(name, this);
    }

    /**
     * Creates a spectrum of colors consisting of "numOfColors" from the minimum
     * wavelength to the maximum wavelength. If the reverse flag is set, then a
     * reverse spectrum is generated. The red, green, and blue indexes are
     * returned through the calling arguments.
     *
     * @param numOfColors
     * @param minWaveLength
     * @param maxWaveLength
     * @param reverse
     */
    private void spread(int numOfColors, float minWaveLength,
            float maxWaveLength, boolean reverse) {
        if (numOfColors <= 0) {
            throw new IllegalArgumentException(
                    "Attempt to create ColorMap containing " + numOfColors
                            + " colors");
        }
        this.colors = new ArrayList<Color>(numOfColors);
        float waveLengthInterval = (maxWaveLength - minWaveLength)
                / numOfColors;
        float wavelength;
        for (int i = 0; i < numOfColors; i++) {
            if (!reverse) {
                wavelength = minWaveLength + (waveLengthInterval * i);
            } else {
                wavelength = maxWaveLength - (waveLengthInterval * i);
            }

            colors.add(getRGBvalue(wavelength));
        }
    }

    /**
     * @param wavelength
     * @return
     */
    private Color getRGBvalue(float wl) {
        double Gamma = 0.80;
        double r = 0, g = 0, b = 0;

        // calculate basic intensities
        if (wl < 380.0) {
            r = g = b = 0.0; // black
        } else if (wl <= 440.0) {
            r = -1.0 * (wl - 440.0) / (440.0 - 380.0);
            g = 0.0;
            b = 1.0;
        } else if (wl <= 490.0) {
            r = 0.0;
            g = (wl - 440.0) / (490.0 - 440.0);
            b = 1.0;
        } else if (wl <= 510.0) {
            r = 0.0;
            g = 1.0;
            b = -1.0 * (wl - 510.0) / (510.0 - 490.0);
        } else if (wl <= 580.0) {
            r = (wl - 510.0) / (580.0 - 510.0);
            g = 1.0;
            b = 0.0;
        } else if (wl < 645.0) {
            r = 1.0;
            g = -1.0 * (wl - 645.0) / (645.0 - 580.0);
            b = 0.0;
        } else if (wl < 780.0) {
            r = 1.0;
            g = 0.0;
            b = 0.0;
        }

        // let the intensity fall off near the vision limits
        double sss = 1.0;
        if (wl > 700.0) {
            sss = 0.3 + 0.7 * (780.0 - wl) / (780.0 - 700.0);
        } else if (wl < 420.0) {
            sss = 0.3 + 0.7 * (wl - 380.0) / (420.0 - 380.0);
        }

        // perform the gamma adjust and intensity adjust
        r = Math.pow(sss * r, Gamma);
        g = Math.pow(sss * g, Gamma);
        b = Math.pow(sss * b, Gamma);

        return new Color((float) r, (float) g, (float) b);
    }

    /**
     * Removes duplicate entries.
     */
    public void removeDuplicates() {
        List<Color> colors = new ArrayList<Color>();
        Color current = null;
        for (Color color: this.colors) {
            if (!color.equals(current)) {
              	colors.add(color);
              	current = color;
            }
        }

        this.colors = colors;
        changed = true;
    }
}
