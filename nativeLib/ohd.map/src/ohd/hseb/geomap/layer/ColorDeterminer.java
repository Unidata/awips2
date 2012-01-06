package ohd.hseb.geomap.layer;

import java.awt.Color;

public interface ColorDeterminer
{
    Color getColorByValue(double value);
    double[] getLevelArray();

}
