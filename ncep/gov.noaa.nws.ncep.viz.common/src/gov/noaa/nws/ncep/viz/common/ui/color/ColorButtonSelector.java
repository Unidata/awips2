/**
 * 
 */
package gov.noaa.nws.ncep.viz.common.ui.color;

import org.eclipse.core.commands.common.EventManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;


/**
 * The <code>ColorButtonSelector</code> can be used in place of JFace <code>ColorSelector</code>
 * to provide a familiar GEMPAK color matrix palette, in addition to a path to the standard OS
 * specific SWT ColorMatrixDialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07 May 2009  74B         B. Hebbard  Initial implementation.
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */
/**
 * @author bhebbard
 *
 */
public class ColorButtonSelector extends EventManager {
	
	// The currently selected color, as SWT RGB object
	private RGB currentRGB = null;
	
	// The previously selected color (if any)
	private RGB oldRGB = null;

	// The currently selected color as SWT Color object (requires disposal care)
	private Color currentColor = null;

	// Property name to use in color change event
    public static final String PROP_COLORCHANGE = "colorValue"; //$NON-NLS-1$
    
	// Composite containing selector
    private final Composite composite;
    
    // The color button itself
    private Button colorButton = null;
    
    /**
     * Constructor:  Create a new instance of the receiver in the supplied
     * parent <code>Composite</code>, using a default size.
     * 
	 * @param parent
     *            A parent Composite in which to put the new color button selector.           
     */
	public ColorButtonSelector(final Composite parent)
	{
		this (parent, 23, 18);  //  use a default size
	}
    
    /**
     * Constructor:  Create a new instance of the receiver in the supplied
     * parent <code>Composite</code>.
     * 
	 * @param parent
     *            A parent Composite in which to put the new color button selector.                      
	 * @param colorButtonWidth
	 *            Width (pixels) of the button.
	 * @param colorButtonHeight
	 *            Height (pixels) of the button.
     */
	public ColorButtonSelector(final Composite parent,
			final int colorButtonWidth,
			final int colorButtonHeight
			)
	{	
		composite = new Composite(parent, SWT.NONE);
	    
        FormLayout parentLayout = new FormLayout();
        composite.setLayout(parentLayout);
        
        colorButton = new Button(composite, SWT.NONE);
        
        FormData formData = new FormData(colorButtonWidth, colorButtonHeight);
        formData.height = colorButtonHeight;
        formData.width = colorButtonWidth;
        colorButton.setLayoutData(formData);
        
	    colorButton.addSelectionListener(new SelectionListener() 
        {
        	public void widgetSelected(SelectionEvent event) 
        	{
                ColorMatrixDialog colorDialog = new ColorMatrixDialog(composite.getShell(),
                		"Color Palette");
                colorDialog.setColor(currentRGB);
                colorButton.setEnabled(false);
                if (colorDialog.open() != Window.CANCEL)  //  no explicit OK; assume unless cancel
                {
    	            setColorValue(colorDialog.getColor());
	                fireColorChangeEvent();
                }
                colorButton.setEnabled(true);
        	}
        	public void widgetDefaultSelected(SelectionEvent event) 
        	{
        	}
        });
	    
	    colorButton.addPaintListener(new PaintListener() {
	    	public void paintControl(PaintEvent event) {
	    		Button button = (Button) event.widget;
	    		button.setBackground(currentColor);
	    	    //  prevent button from turning white on mouseover
	            event.gc.fillRectangle(2,2,colorButtonWidth-4,colorButtonHeight-4);
	    	}
	    });

	}

	/**
	 * Get the button control being wrappered by the selector.
	 *
	 * @return <code>Button</code>
	 */
    public Button getButton() {
	    return colorButton;
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

        //  Update the Color from new RGB; tell canvas to update
        
        if ( currentColor != null )
        {
        	currentColor.dispose();
        }
        currentColor = new Color(composite.getDisplay(), currentRGB);
        colorButton.redraw();
        colorButton.update();
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
	
	public void dispose() {
		if (composite != null){
			composite.dispose();
		}
	}

	public boolean isDisposed() {
		return composite.isDisposed();
	}
}
