package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Spinner;

public class SpinnerSlider  extends Composite {
	
    private Slider scale;

    private Spinner spinner;
    
    private int thumb; 

    private int offset;

    /**
     * Constructs a SliderScale
     * 
     * @param parent
     *            parent shell
     * @param style
     *            SWT.HORIZIONTAL or SWT.VERTICAL
     */
    public SpinnerSlider(Composite parent, int style, int thumb) {
        super(parent, SWT.NONE);

        boolean horizontal = (style & SWT.HORIZONTAL) != 0;
        GridLayout layout = new GridLayout((horizontal ? 2 : 1), false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.horizontalSpacing= 0;
        layout.verticalSpacing = 0;
        setLayout(layout);

        GridData layoutData;
        scale = new Slider(this, style);
        if (horizontal) {
            layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        } else {
            layoutData = new GridData(SWT.CENTER, SWT.FILL, false, true);
        }
        scale.setLayoutData(layoutData);

        scale.setThumb(thumb);
        this.setThumb(thumb);
        
        spinner = new Spinner(this, SWT.BORDER);
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        layoutData.minimumWidth = 30;
        spinner.setLayoutData(layoutData);

        scale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                spinner.setSelection(scale.getSelection() - offset);
            }
        });

        spinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                scale.setSelection(spinner.getSelection() + offset);
            }
        });
    }

    /**
     * Sets the selection, which is the receiver's position, to the argument. If
     * the argument is not within the range specified by minimum and maximum, it
     * will be adjusted to fall within this range.
     * 
     * @param value
     *            the selection
     */
    public void setSelection(int value) {
        scale.setSelection(value + offset);
        spinner.setSelection(value);//* (int)Math.pow(10, spinner.getDigits()) );
    }

    /**
     * Returns the selection, which is the receiver's position.
     * 
     * @return the selection
     */
    public int getSelection() {
        return spinner.getSelection();
    }

    /**
     * Sets the minimum value that the receiver will allow. This new value will
     * be ignored if it is not less than the receiver's current maximum value.
     * If the new minimum is applied then the receiver's selection value will be
     * adjusted if necessary to fall within its new range.
     * 
     * @param value
     *            the new minimum, which must be less than the current maximum
     */
    public void setMinimum(int value) {
        spinner.setMinimum(value);

        int min = spinner.getMinimum();
        if (min < 0) {
            this.offset = -min;
        }

        scale.setMinimum(min + offset);
        scale.setMaximum(spinner.getMaximum() + offset);
    }

    /**
     * Returns the minimum value which the receiver will allow.
     * 
     * @return the minimum
     */
    public int getMinimum() {
        return spinner.getMinimum();
    }

    /**
     * Sets the maximum value that the receiver will allow. This new value will
     * be ignored if it is not greater than the receiver's current minimum
     * value. If the new maximum is applied then the receiver's selection value
     * will be adjusted if necessary to fall within its new range.
     * 
     * @param value
     *            the new maximum, which must be greater than the current
     *            minimum
     */
    public void setMaximum(int value) {
        spinner.setMaximum(value);
        scale.setMaximum(spinner.getMaximum() + thumb);//offset);
    }

    /**
     * Returns the maximum value which the receiver will allow.
     * 
     * @return the maximum
     */
    public int getMaximum() {
        return spinner.getMaximum();
    }

    /**
     * Sets the amount that the receiver's value will be modified by when the
     * up/down arrows are pressed to the argument, which must be at least one.
     * 
     * @param value
     *            the new increment (must be greater than zero)
     */
    public void setIncrement(int value) {
        scale.setIncrement(value);
        spinner.setIncrement(value);
    }

    /**
     * Returns the amount that the receiver's value will be modified by when the
     * up/down arrows are pressed.
     * 
     * @return the increment
     */
    public int getIncrement() {
        return spinner.getIncrement();
    }

    /**
     * Sets the number of decimal places used by the receiver.
     * 
     * The digit setting is used to allow for floating point values in the
     * receiver. For example, to set the selection to a floating point value of
     * 1.37 call setDigits() with a value of 2 and setSelection() with a value
     * of 137. Similarly, if getDigits() has a value of 2 and getSelection()
     * returns 137 this should be interpreted as 1.37. This applies to all
     * numeric APIs.
     * 
     * @param value
     *            the new digits (must be greater than or equal to zero)
     */
    public void setDigits(int value) {
        spinner.setDigits(value);
    }

    /**
     * Returns the number of decimal places used by the receiver.
     * 
     * @return the digits
     */
    public int getDigits() {
        return spinner.getDigits();
    }

    /**
     * Returns the maximum number of characters that the receiver's text field
     * is capable of holding. If this has not been changed by setTextLimit(), it
     * will be the constant Spinner.LIMIT.
     * 
     * @return the text limit
     */
    public int getTextLimit() {
        return spinner.getTextLimit();
    }

    /**
     * Sets the maximum number of characters that the receiver's text field is
     * capable of holding to be the argument. <br>
     * <br>
     * To reset this value to the default, use setTextLimit(Spinner.LIMIT).
     * Specifying a limit value larger than Spinner.LIMIT sets the receiver's
     * limit to Spinner.LIMIT.
     * 
     * @param limit
     *            new text limit
     */
    public void setTextLimit(int limit) {
        spinner.setTextLimit(limit);
    }

    /**
     * Sets the receiver's selection, minimum value, maximum value, digits,
     * increment and page increment all at once. <br>
     * <br>
     * Note: This is similar to setting the values individually using the
     * appropriate methods, but may be implemented in a more efficient fashion
     * on some platforms.
     * 
     * @param selection
     *            the new selection value
     * @param minimum
     *            the new minimum value
     * @param maximum
     *            the new maximum value
     * @param digits
     *            the new digits value
     * @param increment
     *            the new increment value
     * @param pageIncrement
     *            the new pageIncrement value
     */
    public void setValues(int selection, int minimum, int maximum, int digits,
            int increment, int pageIncrement) {
        spinner.setValues(selection, minimum, maximum, digits, increment,
                pageIncrement);

        int min = spinner.getMinimum();
        int max = spinner.getMaximum();
        if (min < 0) {
            this.offset = -min;
        }

        if (max > scale.getMinimum()) {
            scale.setMaximum(max + offset);
            scale.setMinimum(min + offset);
        } else {
            scale.setMinimum(min + offset);
            scale.setMaximum(max + offset);
        }
        scale.setSelection(spinner.getSelection() + offset);
        scale.setIncrement(spinner.getIncrement());
        scale.setPageIncrement(spinner.getPageIncrement());

    }

    /**
     * Sets the amount that the receiver's position will be modified by when the
     * page up/down keys are pressed to the argument, which must be at least
     * one.
     * 
     * @param value
     *            the page increment (must be greater than zero)
     */
    public void setPageIncrement(int value) {
        scale.setPageIncrement(value);
        spinner.setPageIncrement(value);
    }

    /**
     * Returns the amount that the receiver's position will be modified by when
     * the page up/down keys are pressed.
     * 
     * @return the page increment
     */
    public int getPageIncrement() {
        return spinner.getPageIncrement();
    }

    /**
     * Adds the listener to the collection of listeners who will be notified
     * when the control is selected by the user, by sending it one of the
     * messages defined in the SelectionListener interface.
     * 
     * widgetSelected is not called for texts. widgetDefaultSelected is
     * typically called when ENTER is pressed in a single-line text.
     * 
     * @param listener
     *            the listener which should be notified when the control is
     *            selected by the user
     */
    public void addSelectionListener(SelectionListener listener) {
        scale.addSelectionListener(listener);
        spinner.addSelectionListener(listener);
    }

    /**
     * Removes the listener from the collection of listeners who will be
     * notified when the control is selected by the user.
     * 
     * @param listener
     *            the listener which should no longer be notified
     */
    public void removeSelectionListener(SelectionListener listener) {
        scale.removeSelectionListener(listener);
        spinner.removeSelectionListener(listener);
    }
  
 
    public void setThumb(int value){
    	thumb = value;
    }

}
