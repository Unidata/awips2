/**
 * 
 */
package gov.noaa.nws.ncep.viz.common.ui.color;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.common.EventManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;


/**
 * The <code>ColorMatrixSelector</code> can be used in place of JFace <code>ColorSelector</code>
 * to provide a familiar GEMPAK color matrix palette, in addition to a path to the standard OS
 * specific SWT ColorDialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 21 Apr 2009  74/90       B. Hebbard  Initial implementation.
 * 11 May 2009  74B         B. Hebbard  Suppress selectedColorCanvas if requested size zero
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */
/**
 * @author bhebbard
 *
 */
public class ColorMatrixSelector extends EventManager {
	
	// The currently selected color, as SWT RGB object
	private RGB currentRGB = null;
	
	// The previously selected color (if any)
	private RGB oldRGB = null;

	// The currently selected color as SWT Color object (requires disposal care)
	private Color currentColor = null;

	// Property name to use in color change event
    public static final String PROP_COLORCHANGE = "colorValue"; //$NON-NLS-1$

    // A (sparse) mapping of RGB triplets back to the GEMPAK palette colors
	private final Map<RGB, GempakColor> RGBGempakColorMap = new HashMap<RGB, GempakColor>(2*32);

	// Given a GEMPAK color, take us back to its own button
	private final Map<GempakColor, Button> colorButtonMap = new EnumMap<GempakColor, Button>(GempakColor.class);
    
	// Composite given to us by caller in which to place selector
    private final Composite parentComposite;
    
    // The 'demo' area where we show the user the current selected color
    private final Canvas selectedColorCanvas;

    
    /**
     * Constructor:  Create a new instance of the receiver in the supplied
     * parent <code>Composite</code>.
     * 
	 * @param parent
     *            A parent Composite to contain the new color matrix selector.
     *            Note that the receiver will assign a layout to the parent.           
	 * @param horizontalLayout
     *            If true, the preview area, selection matrix, and custom color
     *            button will be laid out horizontally; if false, vertically.
     * @param horizontalMatrix
     *            If true, the color matrix itself will be laid out horizontally;
     *            if false, vertically.
	 * @param selectedColorCanvasHeight
	 *            Height (pixels) of preview area showing the selected color.
	 * @param selectedColorCanvasWidth
	 *            Width (pixels) of preview area showing the selected color.
	 * @param colorButtonHeight
	 *            Height (pixels) of each individual button in the color matrix.
	 * @param colorButtonWidth
	 *            Width (pixels) of each individual button in the color matrix.
	 * @param customButtonHeight
	 *            Height (pixels) of button to bring up the custom color dialog.
	 * @param customButtonWidth
	 *            Width (pixels) of button to bring up the custom color dialog.
	 * @param firstSpace
	 *            Space (pixels) 'before' color preview area (above if
	 *            vertical layout; left if horizontal layout).
	 * @param interSpace
	 *            Space (pixels) between button matrix and both preview area and
	 *            custom color button.
	 * @param colorButtonSpace
	 *            Space (pixels) between individual buttons in matrix, both
	 *            horizontally and vertically.
     */
	public ColorMatrixSelector(final Composite parent,
			final boolean horizontalLayout,
			final boolean horizontalMatrix,
			final int selectedColorCanvasHeight,
			final int selectedColorCanvasWidth,
			final int colorButtonHeight,
			final int colorButtonWidth,
			final int customButtonHeight,
			final int customButtonWidth,
			final int firstSpace,
			final int interSpace,
			final int colorButtonSpace)
	{	
		//TODO:  Think about changing booleans to enums
		parentComposite = parent;
	    
        FormLayout parentLayout = new FormLayout();
        parentComposite.setLayout(parentLayout);

        // (1) The 'demo' area where we show the user the current selected color
        
        selectedColorCanvas = new Canvas(parentComposite, SWT.BORDER);        
        FormData formData1 = new FormData(selectedColorCanvasWidth, selectedColorCanvasHeight);
        if (horizontalLayout)
        {
            formData1.top = new FormAttachment(50, -selectedColorCanvasHeight/2-1);  // center
            formData1.left = new FormAttachment(firstSpace, 0);        	
        }
        else
        {
            formData1.top = new FormAttachment(firstSpace, 0);
            formData1.left = new FormAttachment(50, -selectedColorCanvasWidth/2-2);  // center        	
        }
        selectedColorCanvas.setLayoutData(formData1);
        selectedColorCanvas.addPaintListener(new PaintListener() {
	    	public void paintControl(PaintEvent event) {
	    		Canvas canvas = (Canvas) event.widget;
	    		canvas.setBackground(currentColor);
	    	}
	    });
        if (selectedColorCanvasWidth == 0 && selectedColorCanvasHeight == 0)
        {
        	selectedColorCanvas.setVisible(false);
        }

        // (2) The matrix of color buttons for familiar GEMPAK palette
        
		final Group buttonMatrixGroup = new Group (parentComposite, SWT.NONE);
		
		FormData formData2 = new FormData();
		if (horizontalLayout)
		{			
			/*formData2.top = new FormAttachment(selectedColorCanvas, 0, SWT.CENTER); //TTR 44
			formData2.left = new FormAttachment(selectedColorCanvas, interSpace);*/
			formData2.top = new FormAttachment(5, 0);
			formData2.left = new FormAttachment(firstSpace, 0);
		}
		else
		{			
			formData2.top = new FormAttachment(selectedColorCanvas, interSpace);
			formData2.left = new FormAttachment(selectedColorCanvas, 0, SWT.CENTER);
		}
		buttonMatrixGroup.setLayoutData(formData2);
		
		GridLayout matrixLayout = new GridLayout();
		matrixLayout.numColumns = horizontalMatrix ? 8 : 4;
		matrixLayout.horizontalSpacing = colorButtonSpace;
		matrixLayout.verticalSpacing = colorButtonSpace;
		buttonMatrixGroup.setLayout(matrixLayout);

        for (GempakColor gColor : GempakColor.values())
        {
        	Button colorButton = new Button(buttonMatrixGroup, SWT.TOGGLE);
        	RGBGempakColorMap.put(gColor.getRGB(), gColor);
        	colorButtonMap.put(gColor, colorButton);
            GridData gridData = new GridData();
            gridData.heightHint = colorButtonHeight;
            gridData.widthHint = colorButtonWidth;
            colorButton.setLayoutData(gridData);
        	colorButton.setBackground(new Color(parentComposite.getDisplay(), gColor.getRGB()));
        	colorButton.setToolTipText(gColor.toString().toLowerCase());
        	colorButton.addPaintListener(new PaintListener() {
    	    	public void paintControl(PaintEvent event) {
    	    	//  prevents button from turning white on mouseover
    	    	//  ...but looks awful on RHEL5, so comment it out!  //TODO:  Fix!
    	        //  event.gc.fillRectangle(2,2,colorButtonWidth-4,colorButtonHeight-4);
    	    	}
    	    });
    	    colorButton.addSelectionListener(new SelectionListener() 
	        {
	        	public void widgetSelected(SelectionEvent event) 
	        	{
	        		Button colorButton = (Button) event.widget ;
	        		setColorValue(colorButton.getBackground().getRGB());                   
	        		fireColorChangeEvent();
	        	}
	        	public void widgetDefaultSelected(SelectionEvent event) 
	        	{
	        	}
	        });
        	colorButton.addDisposeListener(new DisposeListener() {
				@Override
				public void widgetDisposed(DisposeEvent event) {
	        		Button colorButton = (Button) event.widget ;
	        		Color color = colorButton.getBackground();
	        		color.dispose();
				}
     	    });
        }

        // (3) A button to take us to a dialog where the user can get any desired color
        
        final Button customColorButton = new Button(parentComposite, SWT.NONE);
        FormData formData3 = new FormData(customButtonWidth, customButtonHeight);
        formData3.height = customButtonHeight;
        formData3.width = customButtonWidth;
        if (horizontalLayout)
        {        	
        	/*formData3.top = new FormAttachment(buttonMatrixGroup, 0, SWT.CENTER); //TTR 44
            formData3.left = new FormAttachment(buttonMatrixGroup, interSpace);*/
            formData3.top = new FormAttachment(80, 0);
            formData3.left=new FormAttachment(firstSpace, 0);  
        }
        else
        {        	
            formData3.top = new FormAttachment(buttonMatrixGroup, interSpace);
            formData3.left = new FormAttachment(buttonMatrixGroup, 0, SWT.CENTER);
        }
        customColorButton.setLayoutData(formData3);
        
      //customColorButton.setText(horizontalLayout ? "..." : "Custom..."); TTR 44
        customColorButton.setText(horizontalLayout ? "More Colors" : "Custom...");
        customColorButton.addSelectionListener(new SelectionListener() 
        {
        	public void widgetSelected(SelectionEvent event) 
        	{
                ColorDialog colorDialog = new ColorDialog(parentComposite.getShell());
                colorDialog.setRGB(currentRGB);
                buttonMatrixGroup.setEnabled(false);
                customColorButton.setEnabled(false);
                RGB returnColor = colorDialog.open();
                buttonMatrixGroup.setEnabled(true);
                customColorButton.setEnabled(true);
                if (returnColor != null)
                {
	        		setColorValue(returnColor);
	        		fireColorChangeEvent();
                }
        	}
        	public void widgetDefaultSelected(SelectionEvent event) 
        	{
        	}
        });
        if (RGBGempakColorMap.get(currentRGB) != null)  //  set initial state
        {
		    colorButtonMap.get(RGBGempakColorMap.get(currentRGB)).setSelection(true);
        }

	}

	
    /**
     * Set the current color value and update the control.
     * 
     * @param newRGB
     *            The new color.
     */
	public void setColorValue (RGB newRGB)
	{
		oldRGB = currentRGB;
		currentRGB = newRGB;
		
    	//  If previous color was a GEMPAK color, make sure its
    	//  matrix button is popped out (deselected)...
		
        if (!currentRGB.equals(oldRGB))
        {
        	GempakColor oldGempakColor = RGBGempakColorMap.get(oldRGB);
            if (oldGempakColor != null)
            {
            	colorButtonMap.get(oldGempakColor).setSelection(false);
            }
        }

        //  ...and if the new color is a GEMPAK color, make sure its
        //  button is pushed.  (Note we need to do this even if the
        //  selection came from a button push; if same as previous color,
        //  SWT.TOGGLE button will pop out if it was already in.)
        
        GempakColor newGempakColor = RGBGempakColorMap.get(currentRGB);
        if (newGempakColor != null)
        {
        	colorButtonMap.get(newGempakColor).setSelection(true);
        }

        //  Update the Color from new RGB; tell canvas to update
        
        if ( currentColor != null )
        {
        	currentColor.dispose();
        }
        currentColor = new Color(parentComposite.getDisplay(), currentRGB);
        selectedColorCanvas.redraw();
        selectedColorCanvas.update();
	}
	
	public RGB getColorValue()
	{
		return currentRGB;
	}
	
    /**
     * Adds a property change listener to this <code>ColorMatrixSelector</code>.
     * Events are fired when the color in the control changes via the user
     * clicking and selecting a new one in the color matrix or the custom color
     * dialog. No event is fired in the case where <code>setColorValue(RGB)</code>
     * is invoked, or if the new color is identical to the old one.
     * 
     * @param listener
     *            A property change listener

     */
	public void addListener(IPropertyChangeListener listener)
	{
        addListenerObject(listener);
	}

    /**
     * Removes the given listener from this <code>ColorMatrixSelector</code>.
     * Has no effect if the listener is not registered.
     * 
     * @param listener
     *            A property change listener
     */
	public void removeListener(IPropertyChangeListener listener)
	{
        removeListenerObject(listener);
	}

	protected void fireColorChangeEvent()
	//  Notify all registered listeners that a color change has occurred.
	{
        if (isListenerAttached() && !currentRGB.equals(oldRGB))
        {
        	PropertyChangeEvent colorChangeEvent =
        		new PropertyChangeEvent(this, PROP_COLORCHANGE, oldRGB, currentRGB);
        	for (Object ls : getListeners())
        	{
        		IPropertyChangeListener listener = (IPropertyChangeListener) ls;
        		listener.propertyChange(colorChangeEvent);
        	}
        }
	}

}
