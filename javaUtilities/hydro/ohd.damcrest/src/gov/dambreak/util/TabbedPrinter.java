package gov.dambreak.util;

/**
 * Class to encapsulate printing from Output Manager.
 * Creation date: (9/22/2003 11:53:09 AM)
 * @author: 
 */
import javax.swing.*;
import java.awt.print.*;
import java.awt.*;


public class TabbedPrinter implements Printable 
{
    private JTabbedPane tabbed;
    private JTable jt;
    private Component c;
    
    
    
    public TabbedPrinter(JTabbedPane _tabbed, JTable _jt) 
    {
        tabbed = _tabbed;
        jt = _jt;
        int index;
        
        PrinterJob pj = PrinterJob.getPrinterJob();
        PageFormat format = pj.pageDialog(pj.defaultPage());
        
        int indexWarnings = tabbed.indexOfTab("Warnings");
        index = tabbed.getSelectedIndex();
        
        // check if the "Warnings" tab appears
        if(indexWarnings == -1)
        {
            if((index == 5) || (index == 6))
            {
                try 
                {
                    format.setOrientation(PageFormat.PORTRAIT); 
                } 
                catch (IllegalArgumentException ia) 
                {
                    System.out.println("Error in format.setOrientation");
                    ia.printStackTrace();
                }
                catch (Exception e) 
                {
                    System.out.println("Error in TabbedPrinter constructor");
                    e.printStackTrace();
                }	
            }
            else
            {
                try 
                {
                    format.setOrientation(PageFormat.LANDSCAPE); 
                } 
                catch (IllegalArgumentException ia) 
                {
                    System.out.println("Error in format.setOrientation");
                    ia.printStackTrace();
                } 
                catch (Exception e) 
                {
                    System.out.println("Error in TabbedPrinter constructor");
                    e.printStackTrace();
                }
            }
        }
        else
        {
            if((index == 6) || (index == 7))
            {
                try 
                {
                    format.setOrientation(PageFormat.PORTRAIT); 
                }
                catch (IllegalArgumentException ia) 
                {
                    System.out.println("Error in format.setOrientation");
                    ia.printStackTrace();
                }
                catch (Exception e) 
                {
                    System.out.println("Error in TabbedPrinter constructor");
                    e.printStackTrace();
                }	
            }
            else
            {
                try 
                {
                    format.setOrientation(PageFormat.LANDSCAPE); 
                }
                catch (IllegalArgumentException ia) 
                {
                    System.out.println("Error in format.setOrientation");
                    ia.printStackTrace();
                }
                catch (Exception e) 
                {
                    System.out.println("Error in TabbedPrinter constructor");
                    e.printStackTrace();
                }
            }
        }
        
        pj.setPrintable(this, format);
        if (!pj.printDialog())
        {
            System.out.println("Error in pj.printDialog");
            return;
        }
        try 
        {
            pj.print();
        }
        catch (PrinterException pe) 
        {
            System.out.println("Error in pj.print");
            pe.printStackTrace();
        }
        catch (Exception e) 
        {
            System.out.println("Error in TabbedPrinter constructor");
            e.printStackTrace();
        }		
    }
    /**
     * Insert the method's description here.
     */
    public int print(Graphics g, PageFormat format, int pagenum) throws java.awt.print.PrinterException 
    {	
        
        String osName = System.getProperty("os.name");
        String lowerCaseName = osName.toLowerCase();
        
        double scale = 1.0;
        int pageIndex = pagenum ;
        int index;
        index = tabbed.getSelectedIndex();
        
        Graphics2D  g2 = (Graphics2D) g;
        g2.setColor(Color.black);
        
        if(index == 1)  //Output Table is printed in the landscape mode
        {
            int fontHeight=g2.getFontMetrics().getHeight();
            int fontDesent=g2.getFontMetrics().getDescent();
            
            
            double pageHeight = format.getImageableHeight()-fontHeight;
            double pageWidth = format.getImageableWidth();
            double tableWidth = (double) jt.getColumnModel().getTotalColumnWidth();
            
            if (tableWidth >= pageWidth) 
            {
                scale =  pageWidth / tableWidth;
            } 
            
            double headerHeightOnPage=
                jt.getTableHeader().getHeight()*scale;
            
            double tableWidthOnPage=tableWidth*scale;
            
            double oneRowHeight=(jt.getRowHeight()+
                    jt.getRowMargin())*scale;
            int numRowsOnAPage=
                (int)((pageHeight-headerHeightOnPage)/oneRowHeight);
            double pageHeightForTable=oneRowHeight*numRowsOnAPage;
            int totalNumPages= (int)Math.ceil((
                    (double)jt.getRowCount())/numRowsOnAPage);
            
            if(pageIndex >= totalNumPages) 
            {
                return NO_SUCH_PAGE;
            }
            
            if (pageIndex + 1 == totalNumPages) 
            {
                int lastRowPrinted = numRowsOnAPage * pageIndex;
                int numRowsLeft = jt.getRowCount() - lastRowPrinted;
                /* g2.setClip(0, (int)(pageHeightForTable * pageIndex),
                 (int) Math.ceil(tableWidthOnPage),
                 (int) Math.ceil(oneRowHeight * numRowsLeft));*/
                
                g2.setClip(50, 50,648,160);
                
            }
            //else clip to the entire area available.
            else
            {    
                g2.setClip(0, (int)(pageHeightForTable*pageIndex), 
                        (int) Math.ceil(tableWidthOnPage),
                        (int) Math.ceil(pageHeightForTable));
            }
            
            g2.scale(scale,scale);
            
            g2.translate(72.0, 72.0);
            
            jt.paint(g2);
            
            /* g2.scale(1/scale,1/scale);
             g2.translate(0f,pageIndex*pageHeightForTable);   	    	
             g2.setClip(0, 0,(int) Math.ceil(tableWidthOnPage), 
             (int)Math.ceil(headerHeightOnPage));	
             g2.scale(scale,scale);*/
            
            g2.translate(0.0, -headerHeightOnPage);
            jt.getTableHeader().paint(g2);
            return Printable.PAGE_EXISTS;
        }
        else
        {
            if (pagenum > 0)
            {
                return Printable.NO_SUCH_PAGE;
            }
            
            int fontHeight=g2.getFontMetrics().getHeight();
            int fontDesent=g2.getFontMetrics().getDescent();
            
            
            double pageHeight = format.getImageableHeight()-fontHeight;
            double pageWidth = format.getImageableWidth();
            double tableWidth = (double) jt.getColumnModel().getTotalColumnWidth();
            
            if (tableWidth >= pageWidth) 
            {
                scale = (pageWidth * 0.95) / tableWidth;
            }
            if(lowerCaseName.indexOf("linux") > -1)
            {
                if((index == 0) || (index == 4))
                {
                    g2.scale(1.0, 0.95);  	// print GUI under tab "Graphics" or 
                                            // GUI under tab "Export to FLDVIEW" on the 
                                            // Linux - portrait mode
                }
            }
            g2.scale(scale, 0.95);
            
            //	  	g2.scale(0.73, 0.95);
            g2.translate((format.getImageableX() + tableWidth * 0.03), format.getImageableY());
            tabbed.paint(g2);
        }
        
        return Printable.PAGE_EXISTS;
    }
    
}
