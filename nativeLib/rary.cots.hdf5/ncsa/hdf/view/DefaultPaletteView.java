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

import ncsa.hdf.object.*;

import java.awt.event.*;

import javax.swing.*;

import java.awt.image.*;

import javax.swing.border.*;
import javax.swing.table.*;
import javax.swing.event.*;
import java.awt.Color;
import java.awt.Image;
import java.awt.Point;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.Graphics;
import java.io.IOException;
import java.util.Vector;

/**
 * To view and change palette.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class DefaultPaletteView extends JDialog
implements PaletteView, MouseListener, MouseMotionListener,
ActionListener, ItemListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private final Color[] lineColors = {Color.red, Color.green, Color.blue};
    private final String lineLabels[] ={"Red", "Green", "Blue"};

    private static String PALETTE_GRAY = "Gray";
    private static String PALETTE_DEFAULT = "Default";
    private static String PALETTE_REVERSE_GRAY = "Reverse Gray";
    private static String PALETTE_GRAY_WAVE = "GrayWave";
    private static String PALETTE_RAINBOW = "Rainbow";
    private static String PALETTE_NATURE = "Nature";
    private static String PALETTE_WAVE = "Wave";

    private JRadioButton checkRed, checkGreen, checkBlue;
    /** Panel that draws plot of data values. */
    private ChartPanel chartP;
    private int x0, y0; // starting point of mouse drag
    private Image originalImage, currentImage;
    boolean isPaletteChanged = false;
    byte[][] palette;
    private ScalarDS dataset;
    private ImageView imageView;
    private int[][] paletteData;
    private JComboBox choicePalette;
    private PaletteValueTable paletteValueTable;

    public DefaultPaletteView(ImageView theImageView)
    {
        this(null, theImageView);
    }

    public DefaultPaletteView(ViewManager theViewer, ImageView theImageView)
    {
        super((JFrame)theViewer, true);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        imageView = theImageView;
        dataset = (ScalarDS)imageView.getDataObject();

        choicePalette = new JComboBox();
        choicePalette.addItemListener(this);
         
        choicePalette.addItem("Select palette");
        choicePalette.addItem(PALETTE_DEFAULT);
        choicePalette.addItem(PALETTE_GRAY);
        choicePalette.addItem(PALETTE_GRAY_WAVE);
        choicePalette.addItem(PALETTE_RAINBOW);
        choicePalette.addItem(PALETTE_NATURE);
        choicePalette.addItem(PALETTE_WAVE);
        Vector plist = ViewProperties.getPaletteList();
        int n = plist.size();
        for (int i=0; i<n; i++)
        	choicePalette.addItem(plist.get(i));

        chartP = new ChartPanel();
        chartP.setBackground(Color.white);

        paletteData = new int[3][256];
        byte[][] imagePalette = imageView.getPalette();
        this.setTitle("Image Palette for - "+dataset.getPath()+dataset.getName());

        int d = 0;
        for (int i=0; i<3; i++)
        {
            for (int j=0; j<256; j++)
            {
                d = imagePalette[i][j];
                if (d < 0) {
                    d += 256;
                }
                paletteData[i][j] = d;
            }
        }

        imageView = theImageView;
        chartP.addMouseListener(this);
        chartP.addMouseMotionListener(this);

        x0 = y0 = 0;
        originalImage = currentImage = imageView.getImage();
        palette = new byte[3][256];

        createUI();
        setVisible(true);
    }

    /** returns the data object displayed in this data viewer */
    public HObject getDataObject() {
        return dataset;
    }

    /**
    *  Creates and layouts GUI componentes.
    */
    private void createUI()
    {
        Window owner = getOwner();

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5, 5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        int w = 700 + (ViewProperties.getFontSize()-12)*15;
        int h = 500 + (ViewProperties.getFontSize()-12)*10;
        contentPane.setPreferredSize(new Dimension(w, h));

        contentPane.add(chartP, BorderLayout.CENTER);

        JButton button = new JButton("  Ok  ");
        button.addActionListener(this);
        button.setActionCommand("Ok");
        JPanel buttonP = new JPanel();
        buttonP.setBorder(new LineBorder(Color.GRAY));
        buttonP.add(button);
        button = new JButton("Cancel");
        button.addActionListener(this);
        button.setActionCommand("Cancel");
        buttonP.add(button);
        button = new JButton("Preview");
        button.addActionListener(this);
        button.setActionCommand("Preview");
        buttonP.add(button);

        JPanel bottomP = new JPanel();
        bottomP.setLayout(new BorderLayout(20, 2));
        bottomP.add(buttonP, BorderLayout.EAST);

        checkRed = new JRadioButton("Red");
        checkRed.setForeground(Color.red);
        checkGreen = new JRadioButton("Green");
        checkGreen.setForeground(Color.green);
        checkBlue = new JRadioButton("Blue");
        checkBlue.setForeground(Color.blue);
        checkRed.setSelected(true);
        ButtonGroup bgroup = new ButtonGroup();
        bgroup.add(checkRed);
        bgroup.add(checkGreen);
        bgroup.add(checkBlue);
        JPanel checkP = new JPanel();
        checkP.setBorder(new LineBorder(Color.GRAY));
        checkP.add(checkRed);
        checkP.add(checkGreen);
        checkP.add(checkBlue);
        bottomP.add(checkP, BorderLayout.WEST);

        JPanel valueP = new JPanel();
        valueP.setLayout(new GridLayout(1, 2));
        valueP.setBorder(new LineBorder(Color.GRAY));
        JButton valueButton = new JButton("Show Values");
        valueButton.setActionCommand("Show palette values");
        valueButton.addActionListener(this);
        valueP.add(choicePalette);
        valueP.add(valueButton);
        bottomP.add(valueP, BorderLayout.CENTER);

        contentPane.add(bottomP, BorderLayout.SOUTH);

        Point l = owner.getLocation();
        l.x += 350;
        l.y += 200;
        setLocation(l);
        pack();
    }

    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();

        if (cmd.equals("Ok"))
        {
            if (isPaletteChanged)
            {
                this.updatePalette();
                isPaletteChanged = false;
                imageView.setPalette(palette);
                imageView.setImage(currentImage);
            }
            super.dispose();
        }
        else if (cmd.equals("Cancel"))
        {
            imageView.setImage(originalImage);
            super.dispose();
        }
        else if (cmd.equals("Preview"))
        {
            this.updatePalette();
            imageView.setImage(currentImage);
        }
        else if (cmd.equals("Show palette values"))
        {
            if (paletteValueTable == null) {
                paletteValueTable = new PaletteValueTable(this);
            }
            paletteValueTable.refresh();
            paletteValueTable.setVisible(true);
        }
        else if (cmd.equals("Hide palette values"))
        {
            if (paletteValueTable != null) {
                paletteValueTable.setVisible(false);
            }
        }
    }

    public void dispose() {
        imageView.setImage(originalImage);
        super.dispose();
    }

    public void itemStateChanged(ItemEvent e) {
        Object src = e.getSource();

        if (!src.equals(choicePalette)) {
            return;
        }

        int idx = choicePalette.getSelectedIndex();
        if ( idx<=0) {
            return;
        }

        byte[][] imagePalette = null;
        Object item = choicePalette.getSelectedItem();
        
        if ( item.equals(PALETTE_DEFAULT) ) {
            imagePalette = dataset.getPalette();
        }
        else if ( item.equals(PALETTE_GRAY) ) {
            imagePalette = Tools.createGrayPalette();
        }
        else if ( item.equals(PALETTE_REVERSE_GRAY) ) {
            imagePalette = Tools.createReverseGrayPalette();
        } else if ( item.equals(PALETTE_GRAY_WAVE) ) {
            imagePalette = Tools.createGrayWavePalette();
        } else if ( item.equals(PALETTE_RAINBOW) ) {
            imagePalette = Tools.createRainbowPalette();
        } else if ( item.equals(PALETTE_NATURE) ) {
            imagePalette = Tools.createNaturePalette();
        } else if ( item.equals(PALETTE_WAVE) ) {
            imagePalette = Tools.createWavePalette();
        } else {
        	byte[][] pal = Tools.readPalette((String)item);
        	if (pal != null)
        		imagePalette = pal;
        }
        
        if (imagePalette == null) {
            return;
        }

        int d = 0;
        for (int i=0; i<3; i++)
        {
            for (int j=0; j<256; j++)
            {
                d = imagePalette[i][j];
                if (d < 0) {
                    d += 256;
                }
                paletteData[i][j] = d;
            }
        }

        chartP.repaint();
        isPaletteChanged = true;

    }

    private void updatePalette()
    {
        for (int i=0; i<256; i++)
        {
            palette[0][i] = (byte)paletteData[0][i];
            palette[1][i] = (byte)paletteData[1][i];
            palette[2][i] = (byte)paletteData[2][i];
        }

        IndexColorModel colorModel = new IndexColorModel (
            8,           // bits - the number of bits each pixel occupies
            256,         // size - the size of the color component arrays
            palette[0],  // r - the array of red color components
            palette[1],  // g - the array of green color components
            palette[2]); // b - the array of blue color components

        int w = dataset.getWidth();
        int h = dataset.getHeight();
        MemoryImageSource memoryImageSource = null;
        
        try { memoryImageSource = (MemoryImageSource)originalImage.getSource(); }
        catch (Throwable err) { memoryImageSource = null; }
        
        if (memoryImageSource == null) {
            memoryImageSource = new MemoryImageSource(w, h, colorModel, imageView.getImageByteData(), 0, w);
        } else {
            memoryImageSource.newPixels(imageView.getImageByteData(), colorModel, 0, w);
        }
        
        currentImage = Toolkit.getDefaultToolkit().createImage (memoryImageSource);        
    }

    public void mouseClicked(MouseEvent e){} // MouseListener
    public void mouseReleased(MouseEvent e) {
        if ((paletteValueTable != null) && paletteValueTable.isVisible()) {
            paletteValueTable.refresh();
        }
    } // MouseListener
    public void mouseEntered(MouseEvent e) {} // MouseListener
    public void mouseExited(MouseEvent e)  {} // MouseListener
    public void mouseMoved(MouseEvent e) {} // MouseMotionListener

    // implementing MouseListener
    public void mousePressed(MouseEvent e)
    {
        //x0 = e.getX()-40; // takes the horizontal gap
        //if (x0 < 0) x0 = 0;
        //y0 = e.getY()+20;
    }

    // implementing MouseMotionListener
    public void mouseDragged(MouseEvent e)
    {
        int x1 = e.getX()-40;// takes the vertical gap
        if (x1< 0) {
            x1 = 0;
        }
        int y1 = e.getY()+20;

        Dimension d = chartP.getSize();
        double ry = 255/(double)d.height;
        double rx = 255/(double)d.width;

        int lineIdx = 0;
        if (checkGreen.isSelected()) {
            lineIdx = 1;
        } else if (checkBlue.isSelected()) {
            lineIdx = 2;
        }

        int idx = 0;
        double b = (double)(y1-y0)/(double)(x1-x0);
        double a = y0-b*x0;
        double value = y0*ry;
        int i0 = Math.min(x0, x1);
        int i1 =  Math.max(x0, x1);
        for (int i=i0; i<i1; i++)
        {
            idx = (int)(rx*i);
            if (idx > 255) {
                continue;
            }
            value = 255-(a+b*i)*ry;
            if (value < 0) {
                value = 0;
            } else if (value > 255) {
                value = 255;
            }
            paletteData[lineIdx][idx] = (int)value;
        }

        chartP.repaint();
        isPaletteChanged = true;
    }

    /** The dialog to show the palette values in spreadsheet. */
    private final class PaletteValueTable extends JDialog
    {
    	public static final long serialVersionUID = HObject.serialVersionUID;

        private JTable valueTable;
        private DefaultTableModel valueTableModel;
        String rgbName = "Color";
        String idxName = "Index";
        private final boolean startEditing[] = {false};

        public PaletteValueTable(DefaultPaletteView owner)
        {
            super(owner);
            String[] columnNames = {idxName, "Red", "Green", "Blue", rgbName};
            valueTableModel = new DefaultTableModel(columnNames, 256);

            valueTable = new JTable(valueTableModel)
            {
            	public static final long serialVersionUID = HObject.serialVersionUID;

                public boolean isCellEditable(int row, int col)
                {
                    return (col > 0 && col < 4);
                }

                public Object getValueAt(int row, int col)
                {
                	if (startEditing[0])
                		return "";
                	
                	if (col==0)
                		return String.valueOf(row);
                	else if (col <4) {
                        return String.valueOf(paletteData[col-1][row]);
                    } else {
                        return "";
                    }
                }
                
                public boolean editCellAt(int row, int column, java.util.EventObject e) 
                {
                	
                    if (!isCellEditable(row, column)) {
                        return super.editCellAt(row, column, e);
                    }
                    
    				if (e instanceof KeyEvent) {
    					KeyEvent ke = (KeyEvent)e;
    					if (ke.getID()==KeyEvent.KEY_PRESSED) 
    						startEditing[0] = true;
    				}
    				
                    return super.editCellAt(row, column, e);
                }
                
                public void editingStopped(ChangeEvent e)
                {
                    int row = getEditingRow();
                    int col = getEditingColumn();

                    if (!isCellEditable(row, col)) {
                        return;
                    }

                    String oldValue = (String)getValueAt(row, col);
                    super.editingStopped(e);
                    startEditing[0] = false;

                    Object source = e.getSource();

                    if (source instanceof CellEditor)
                    {
                        CellEditor editor = (CellEditor)source;
                        String newValue = (String)editor.getCellEditorValue();
                        setValueAt(oldValue, row, col); // set back to what it is
                        updatePaletteValue(newValue, row, col-1);
                    }
                }
            };

            valueTable.getColumn(rgbName).setCellRenderer(new DefaultTableCellRenderer()
            {
            	public static final long serialVersionUID = HObject.serialVersionUID;

                Color color = Color.white;
                public java.awt.Component getTableCellRendererComponent(JTable table,
                       Object value, boolean isSelected, boolean hasFocus, int row, int col)
                {
                    java.awt.Component comp = super.getTableCellRendererComponent(table,  value, isSelected, hasFocus, row, col);
                    color = new Color(paletteData[0][row], paletteData[1][row], paletteData[2][row]);
                    comp.setBackground(color);
                    return comp;
                }
            });
            
            valueTable.getColumn(idxName).setCellRenderer(new DefaultTableCellRenderer()
            {
            	public static final long serialVersionUID = HObject.serialVersionUID;
                public java.awt.Component getTableCellRendererComponent(JTable table,
                       Object value, boolean isSelected, boolean hasFocus, int row, int col)
                {
                    java.awt.Component comp = super.getTableCellRendererComponent(table,  value, isSelected, hasFocus, row, col);
                    comp.setBackground(Color.LIGHT_GRAY);
                    return comp;
                }
            });

            valueTable.setRowSelectionAllowed(false);
            valueTable.setCellSelectionEnabled(true);
            valueTable.getTableHeader().setReorderingAllowed(false);
            valueTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            
            // set cell height for large fonts
    		int cellRowHeight = Math.max(16, valueTable.getFontMetrics(valueTable.getFont()).getHeight());
             valueTable.setRowHeight(cellRowHeight);

            JScrollPane scroller = new JScrollPane(valueTable);

            JPanel contentPane = (JPanel)getContentPane();
            int w = 300 + (ViewProperties.getFontSize()-12)*10;
            int h = 600 + (ViewProperties.getFontSize()-12)*15;
            contentPane.setPreferredSize(new Dimension(w, h));
            contentPane.setLayout(new BorderLayout(5, 5));
            contentPane.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
            contentPane.add(scroller, BorderLayout.CENTER);

            JButton button = new JButton("  Ok  ");
            button.addActionListener(owner);
            button.setActionCommand("Hide palette values");
            
            JPanel tmpP = new JPanel();
            tmpP.add(button);
            contentPane.add(tmpP, BorderLayout.SOUTH);

            Point l = owner.getLocation();
            l.x += 100;
            l.y += 100;
            setLocation(l);
            pack();
        }

        private void updatePaletteValue(String strValue, int row, int col)
        {
            if (strValue == null) {
                return;
            }

            int value = 0;
            
            try { 
            	value = Integer.parseInt(strValue);
            } catch (Exception ex) { 
            	return;
            }
            
            if (value < 0 || value > 255) {
                JOptionPane.showMessageDialog(
                this,
                "Value is out of range [0, 255]\n",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
                return;
            }
            
            paletteData[col][row] = value;
            chartP.repaint();
            isPaletteChanged = true;
        }

        public void refresh()
        {
            valueTable.editingStopped(new ChangeEvent (valueTable));
            valueTable.updateUI();
        }
    }

    /** The canvas that paints the data lines. */
    private final class ChartPanel extends JComponent
    {
    	public static final long serialVersionUID = HObject.serialVersionUID;

        /**
        * Paints the plot components.
        */
        public void paint(Graphics g)
        {
            Dimension d = getSize();
            int gap = 20;
            int legendSpace = 60;
            int h = d.height - gap;
            int w = d.width - 3*gap - legendSpace;

            // draw the X axis
            g.drawLine(2*gap, h, w+2*gap, h);

            // draw the Y axis
            g.drawLine(2*gap, h, 2*gap, 0);

            // draw X and Y labels: 10 labels for x and y
            int dh = h/10;
            int dw = w/10;
            int dx = 25;
            double dy = 25;
            int xp=2*gap, yp=0, x=0, x0, y0, x1, y1;
            double y = 0;

            // draw X and Y grid labels
            g.drawString(String.valueOf((int)y), 0, h+8);
            g.drawString(String.valueOf(x), xp-5, h+gap);
            for (int i=0; i<10; i++)
            {
                xp += dw;
                yp += dh;
                x += dx;
                y += dy;
                g.drawLine(xp, h, xp, h-5);
                g.drawLine(2*gap, h-yp, 2*gap+5, h-yp);
                g.drawString(String.valueOf((int)y), 0, h-yp+8);
                g.drawString(String.valueOf(x), xp-5, h+gap);
            }

            Color c = g.getColor();
            for (int i=0; i<3; i++) {
                g.setColor(lineColors[i]);

                // set up the line data for drawing one line a time
                for (int j=0; j<255; j++)
                {
                    x0 = (w*j/255) + 2*gap;
                    y0 = (h - h*paletteData[i][j]/255);
                    x1 = (w*(j+1)/255) + 2*gap;
                    y1 = (h - h*(paletteData[i][j+1])/255);
                    g.drawLine(x0, y0, x1, y1);
                }

                x0 = w+legendSpace;
                y0 = gap+gap*i;
                g.drawLine(x0, y0, x0+7, y0);
                g.drawString(lineLabels[i], x0+10, y0+3);
            }

            g.setColor(c); // set the color back to its default

            // draw a box on the legend
            g.drawRect(w+legendSpace-10, 10, legendSpace, 10*gap);
        } // public void paint(Graphics g)

    } // private class ChartPanel extends Canvas

} // private class PaletteView extends ChartView

