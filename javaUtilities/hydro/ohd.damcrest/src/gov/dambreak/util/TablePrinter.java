package gov.dambreak.util;

/**
 * Insert the type's description here.
 * Creation date: (8/4/2003 11:53:09 AM)
 * @author: 
 */
import javax.swing.*;
import java.awt.print.*;
import java.awt.*;

public class TablePrinter implements Printable 
{
    private JTable table;
    /**
     * Insert the method's description here.
     * Creation date: (8/4/2003 11:58:23 AM)
     * @param table javax.swing.JTable
     * @param tm javax.swing.table.TableModel
     */
    public TablePrinter(JTable _table) 
    {
        table = _table;
        
        PrinterJob pj = PrinterJob.getPrinterJob();
        pj.setPrintable(TablePrinter.this);
        if (!pj.printDialog())
            return;
        try {
            pj.print();
        } catch (Exception PrintException) {}
    }
    /**
     * Insert the method's description here.
     */
    public int print(Graphics g, PageFormat pageFormat, int pageIndex) throws java.awt.print.PrinterException 
    {
        
        Graphics2D  g2 = (Graphics2D) g;
        g2.setColor(Color.black);
        int fontHeight=g2.getFontMetrics().getHeight();
        int fontDesent=g2.getFontMetrics().getDescent();
        
        //leave room for page number
        double pageHeight = pageFormat.getImageableHeight()-fontHeight;
        double pageWidth = pageFormat.getImageableWidth();
        double tableWidth = (double) table.getColumnModel().getTotalColumnWidth();
        double scale = 1; 
        if (tableWidth >= pageWidth) 
        {
            scale =  pageWidth / tableWidth;
        }
        
        double headerHeightOnPage = table.getTableHeader().getHeight() * scale;
        double tableWidthOnPage=tableWidth*scale;
        
        double oneRowHeight=(table.getRowHeight() + table.getRowMargin())*scale;
        int numRowsOnAPage=
            (int)((pageHeight-headerHeightOnPage)/oneRowHeight);
        double pageHeightForTable=oneRowHeight*numRowsOnAPage;
        int totalNumPages= (int)Math.ceil((
                (double)table.getRowCount())/numRowsOnAPage);
        if(pageIndex>=totalNumPages) 
        {
            return Printable.NO_SUCH_PAGE;
        }
        
        g2.translate(pageFormat.getImageableX(), 
                pageFormat.getImageableY());
        g2.drawString("Page: "+(pageIndex+1),(int)pageWidth/2-35,
                (int)(pageHeight+fontHeight-fontDesent));//bottom center
        
        g2.translate(0f,headerHeightOnPage);
        g2.translate(0f,-pageIndex*pageHeightForTable);
        
        //If this piece of the table is smaller than the size available,
        //clip to the appropriate bounds.
        if (pageIndex + 1 == totalNumPages) 
        {
            int lastRowPrinted = numRowsOnAPage * pageIndex;
            int numRowsLeft = table.getRowCount() - lastRowPrinted;
            g2.setClip(0, (int)(pageHeightForTable * pageIndex),
                          (int) Math.ceil(tableWidthOnPage),
                          (int) Math.ceil(oneRowHeight * numRowsLeft));
        }
        //else clip to the entire area available.
        else
        {    
            g2.setClip(0, (int)(pageHeightForTable*pageIndex), 
                          (int) Math.ceil(tableWidthOnPage),
                          (int) Math.ceil(pageHeightForTable));        
        }
        
        g2.scale(scale,scale);
        table.paint(g2);
        g2.scale(1/scale,1/scale);
        g2.translate(0f,pageIndex*pageHeightForTable);
        g2.translate(0f, -headerHeightOnPage);
        g2.setClip(0, 0,(int) Math.ceil(tableWidthOnPage), 
                (int)Math.ceil(headerHeightOnPage));
        g2.scale(scale,scale);
        table.getTableHeader().paint(g2);//paint header at top
        
        return Printable.PAGE_EXISTS;
    }
}
