package gov.noaa.nws.ncep.viz.common.ui.color;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;

/**
 * Provides a migration of the NMAP "Color Palette" dialog; presents
 * a ColorMatrixSelector in a standalone dialog.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07 May 2009  74B         bhebbard    Initial Creation.
 * 11 Dec 2009              bhebbard    Extend (jface) Dialog rather than RTS CaveJFACEDialog,
 *                                      to avoid background repaint (and grey buttons) in TO11D6.
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */

public class ColorMatrixDialog extends Dialog {

	//  Current attribute values.
	    
    private RGB color = new RGB(155, 155, 155);
    
    private String title = "Color Palette";

    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ColorMatrixDialog(Shell parentShell, String dialogTitle) {
        super(parentShell);
        this.title = dialogTitle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    protected void buttonPressed(int buttonId) {
        super.buttonPressed(buttonId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
     */
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
     */
    protected void createButtonsForButtonBar(Composite parent) {

        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);

    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    protected Control createDialogArea(final Composite parent) {

    	Composite composite = (Composite) super.createDialogArea(parent);
        
        FormLayout layout0 = new FormLayout();
        composite.setLayout(layout0);
	    
        //  Line Color

        //TODO:  Remove following line when ColorMatrixSelector no longer requires
        //       this dorky allocation of a separate composite by the parent...
        Composite colorGroup = new Composite(composite, SWT.NONE);
        /*final ColorMatrixSelector cms = new ColorMatrixSelector(colorGroup, true, true,
		0, 0, 18, 22, 28, 28, 0, 6, 4);*/ //TTR 44
        final ColorMatrixSelector cms = new ColorMatrixSelector(colorGroup, true, true,
		0, 0, 18, 22, 28, 108, 0, 6, 4);
        //TODO:  (continued)...and then use the following line instead...
        //final ColorMatrixSelector cms = new ColorMatrixSelector(composite, true, true,
		//		0, 0, 18, 22, 28, 28, 0, 6, 4);
	    cms.setColorValue(color);
	    cms.addListener(new IPropertyChangeListener() {
	    	public void propertyChange(PropertyChangeEvent event) {
	    		color = cms.getColorValue();
	    	    //  dialog has no explicit "OK"; close immediately on color change 
	    		close();
	        }
	    });
	    
	    applyDialogFont(composite);
        return composite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected Point getInitialSize() {
   //     return new Point(272, 186);//TTR 44
        return new Point(238, 226);
    }
        
    public RGB getColor()
    {
    	return color;
    }
        
    public void setColor (RGB color)
    {
    	this.color = color;
    }
}

