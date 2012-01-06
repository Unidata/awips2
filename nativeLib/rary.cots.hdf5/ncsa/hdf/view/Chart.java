/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.view;

import java.awt.event.*;
import javax.swing.*;

import ncsa.hdf.object.HObject;

import java.lang.reflect.Array;
import java.awt.Color;
import java.awt.Frame;
import java.awt.Window;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Dimension;
import java.awt.BorderLayout;

/**
 * ChartView displays histogram/line chart of selected row/column of table
 * data or image. There are two types of chart, histogram and line plot.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class Chart extends JDialog
implements ActionListener
{	
	public static final long serialVersionUID = HObject.serialVersionUID;

    /** histogram style chart */
    public static final int HISTOGRAM = 0;

    /** line style chart */
    public static final int LINEPLOT = 1;

    /** The default colors of lines for selected columns */
    public static final Color[] LINE_COLORS = {
        Color.black, Color.red, Color.green.darker(), Color.blue, Color.magenta,
        Color.pink, Color.yellow, Color.orange, Color.gray, Color.cyan};

    /** the data values of line points or histogram */
    protected double data[][];

    /** Panel that draws plot of data values. */
    protected ChartPanel chartP;

    /** number of data points */
    protected int numberOfPoints;

    /** the style of chart: histogram or line */
    private int chartStyle;

    /** the maximum value of the Y axis */
    private double ymax;

    /** the minumum value of the Y axis */
    private double ymin;

    /** the maximum value of the X axis */
    private double xmax;

    /** the minumum value of the X axis */
    private double xmin;

    /** line labels */
    private String lineLabels[];

    /** line colors */
    private Color lineColors[];

    /** number of lines */
    private int numberOfLines;

    /* the data to plot against */
    private double[] xData=null;

    /**
     * True if the original data is integer (byte, short, integer, long).
     */
    private boolean isInteger;

    private java.text.DecimalFormat format;

    /**
     *  Constructs a new ChartView given data and data ranges.
     *  <p>
     *  @param owner the owner frame of this dialog.
     *  @param title the title of this dialog.
     *  @param style the style of the chart. Valid values are: HISTOGRAM and LINE
     *  @param data the two dimensional data array: data[linenumber][datapoints]
     *  @param xData the range of the X values, xRange[0]=xmin, xRange[1]=xmax.
     *  @param yRange the range of the Y values, yRange[0]=ymin, yRange[1]=ymax.
     */
    public Chart (Frame owner, String title, int style,
        double[][] data, double[] xData, double[] yRange)
    {
        super(owner, title, false);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        format = new java.text.DecimalFormat("0.00E0");

        if (data == null) {
            return;
        }

        this.chartStyle = style;
        this.data = data;

        if (style == HISTOGRAM) {
            isInteger = true;
        } else {
            isInteger = false;
        }

        if (xData != null)
        {
            int len = xData.length;
            if (len == 2)
            {
                this.xmin = xData[0];
                this.xmax = xData[1];
            } else {
                this.xData = xData;
                xmin = xmax = xData[0];
                for (int i=0; i<len; i++)
                {
                    if (xData[i] < xmin) {
                        xmin = xData[i];
                    }

                    if (xData[i] > xmax) {
                        xmax = xData[i];
                    }
                }
            }
        }
        else
        {
            this.xmin = 1;
            this.xmax = data[0].length;
        }

        this.numberOfLines = Array.getLength(data);
        this.numberOfPoints = Array.getLength(data[0]);
        this.lineColors = LINE_COLORS;

        if (yRange != null)
        {
            // data range is given
            this.ymin = yRange[0];
            this.ymax = yRange[1];
        }
        else
        {
            // search data range from the data
            findDataRange();
        }

        if ( (ymax > 0.000001) && (ymax < 1000000)) {
            format = new java.text.DecimalFormat("0.######");
        }

        chartP = new ChartPanel();
        chartP.setBackground(Color.white);

        createUI();
    }

    /**
     *  Creates and layouts GUI componentes.
     */
    protected void createUI()
    {
        Window owner = getOwner();

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5, 5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        int w = 640 + (ViewProperties.getFontSize()-12)*15;
        int h = 400 + (ViewProperties.getFontSize()-12)*10;
        
        contentPane.setPreferredSize(new Dimension(w, h));

        contentPane.add(chartP, BorderLayout.CENTER);

        JButton button = new JButton("Close");
        button.addActionListener(this);
        button.setActionCommand("Close");
        JPanel tmp = new JPanel();
        tmp.add(button);
        contentPane.add(tmp, BorderLayout.SOUTH);

        Point l = owner.getLocation();
        l.x += 220;
        l.y += 100;
        setLocation(l);
        pack();
    }

    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();

        if (cmd.equals("Close"))
        {
            dispose();
        }
    }

    /** Sets the color of each line of a line plot */
    public void setLineColors(Color c[]) { lineColors = c; }

    /** Sets the labels of each line. */
    public void setLineLabels(String l[]) { lineLabels = l; }

    /** Set the data type of the plot data to be integer. */
    public void setTypeToInteger() { isInteger = true; }

    /** find and set the minimum and maximum values of the data */
    private void findDataRange()
    {
        if (data == null) {
            return;
        }

        ymin = ymax = data[0][0];
        for (int i=0; i<numberOfLines; i++)
        {
            for (int j=0; j<numberOfPoints; j++)
            {
                if (data[i][j] < ymin) {
                    ymin = data[i][j];
                }

                if (data[i][j] > ymax) {
                    ymax = data[i][j];
                }
            }
        }
    }

    /** The canvas that paints the data lines. */
    private class ChartPanel extends JComponent
    {
    	public static final long serialVersionUID = HObject.serialVersionUID;

       /**
        * Paints the plot components.
        */
        public void paint(Graphics g)
        {
            if ( numberOfLines <= 0 ) {
                return; // no data
            }

            Dimension d = getSize();
            int gap = 20;
            int xgap = 2*gap;
            int ygap = 2*gap;
            int legendSpace=0;
            if ((chartStyle == LINEPLOT) && (lineLabels != null)) {
                legendSpace = 60;
            }

            int h = d.height - gap;
            int w = d.width - (3*gap + legendSpace);
            int xnpoints = Math.min(10, numberOfPoints-1);
            int ynpoints = 10;

            // draw the X axis
            g.drawLine(xgap, h, w+xgap, h);

            // draw the Y axis
            g.drawLine(ygap, h, ygap, 0);

            // draw x labels
            double xp=0, x=xmin;
            double dw = (double)w/(double)xnpoints;
            double dx = (xmax - xmin)/xnpoints;
            boolean gtOne = (dx>=1);
            for (int i=0; i<=xnpoints; i++)
            {
                x = xmin+i*dx;
                xp = xgap + i*dw;
                g.drawLine((int)xp, h, (int)xp, h-5);
                if (gtOne) {
                    g.drawString(String.valueOf((int)x), (int)xp-5, h+gap);
                } else {
                    g.drawString(String.valueOf(x), (int)xp-5, h+gap);
                }
            }

            // draw y labels
            double yp=0, y=ymin;
            double dh = (double)h/(double)ynpoints;
            double dy = (ymax - ymin) /(ynpoints);
            if (dy > 1) {
                dy = Math.round(dy*10.0)/10.0;
            }
            for (int i=0; i<=ynpoints; i++)
            {
                yp = i*dh;
                y = i*dy+ymin;
                g.drawLine(ygap, h-(int)yp, ygap+5, h-(int)yp);
                if (isInteger) {
                    g.drawString(String.valueOf((int)y), 0, h-(int)yp+8);
                } else {
                    g.drawString(format.format(y), 0, h-(int)yp+8);
                }
            }

            Color c = g.getColor();
            double x0, y0, x1, y1;
            if (chartStyle == LINEPLOT)
            {
                dw = (double)w /(double)(numberOfPoints-1);

                // use y = a + b* x to calculate pixel positions
                double b = h/(ymin-ymax);
                double a = -b*ymax;
                boolean hasXdata = ((xData != null) && (xData.length>=numberOfPoints));
                double xRatio = (1/(xmax-xmin))*w;
                double xD = (xmin/(xmax-xmin))*w;

                // draw lines for selected spreadsheet columns
                for (int i=0; i<numberOfLines; i++)
                {
                    if ((lineColors != null) && (lineColors.length>= numberOfLines)) {
                        g.setColor(lineColors[i]);
                    }

                    // set up the line data for drawing one line a time
                    if (hasXdata) {
                        x0 = xgap + xData[0]*xRatio - xD;
                    } else {
                        x0 = xgap;
                    }
                    y0 = a+b*data[i][0];

                    for (int j=1; j<numberOfPoints; j++)
                    {
                        if (hasXdata) {
                            x1 = xgap + xData[j]*xRatio - xD;
                        } else {
                            x1 = xgap + j*dw;
                        }

                        y1 = a+b*data[i][j];
                        g.drawLine((int)x0, (int)y0, (int)x1, (int)y1);

                        x0 = x1; y0=y1;
                    }

                    // draw line legend
                    if ((lineLabels != null) && (lineLabels.length>= numberOfLines))
                    {
                        x0 = w+legendSpace;
                        y0 = gap+gap*i;
                        g.drawLine((int)x0, (int)y0, (int)x0+7, (int)y0);
                        g.drawString(lineLabels[i], (int)x0+10, (int)y0+3);
                    }
                }

                g.setColor(c); // set the color back to its default

                // draw a box on the legend
                if ((lineLabels != null) && (lineLabels.length>= numberOfLines)) {
                    g.drawRect(w+legendSpace-10, 10, legendSpace, 10*gap);
                }

            } // if (chartStyle == LINEPLOT)
            else if (chartStyle == HISTOGRAM)
            {
                // draw histogram for selected image area
                xp=xgap;
                yp=0;
                g.setColor(Color.blue);
                int barWidth = w/numberOfPoints;
                if (barWidth <=0 ) {
                    barWidth = 1;
                }
                dw = (double)w/(double)numberOfPoints;
                for (int j=0; j<numberOfPoints; j++)
                {
                    xp = xgap + j*dw; 
                    yp = (int)(h*(data[0][j]-ymin)/(ymax-ymin));
                    g.fillRect((int)xp, (int)(h-yp), barWidth, (int)yp);
                }

                g.setColor(c); // set the color back to its default
            } // else if (chartStyle == HISTOGRAM)
        } // public void paint(Graphics g)
    } // private class ChartPanel extends Canvas

}
