/*
 * Created on Nov 16, 2004
 *
 * 
 */
package ohd.hseb.util.gui;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import javax.swing.BoundedRangeModel;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeListener;

import ohd.hseb.util.MathHelper;

/**
 * @author GobsC
 *
 * 
 */
public class LabeledSlider extends JPanel
{
    
	private JSlider   _slider = null;
    private JLabel    _label = null;
	private JLabel    _valueLabel = null;
	
	
	private double _initialModelValue = 0.0;
	private double _minModelValue = 0.0;
	private double _maxModelValue = 1.0;
	
	private int _sliderMin = 0;
	private int _sliderMax = 100;
	
	private NumberFormat _numberFormat = new DecimalFormat("###.00");
	
	
//	 ----------------------------------------------------------------------------
	  
	public LabeledSlider(String labelString, 
	                     double initialModelValue,
	                     double minModelValue,
	                     double maxModelValue,
	                     int width,
	                     int height)
	{
	    
	        //set the private data member variables
	    
	        _label = new JLabel(labelString);
	        
	        _initialModelValue = initialModelValue;
	        _minModelValue = minModelValue;
	        _maxModelValue = maxModelValue;
	   
	        
	        //initialize the private gui variables
	        
	        int initialSliderValue = getSliderValueFromModelValue(initialModelValue);
	        _valueLabel = new JLabel(getSliderValueLabelString(initialModelValue));
	        
	        BoundedRangeModel boundedRangeModel = new DefaultBoundedRangeModel();
	        boundedRangeModel.setMinimum(_sliderMin);
	        boundedRangeModel.setMaximum(_sliderMax);
	        boundedRangeModel.setValue(initialSliderValue);
	        _slider = new JSlider(boundedRangeModel);
	        
	        //add gui elements
	        this.add(_label);
	        this.add(_slider);
	        this.add(_valueLabel);
	        
	        this.setSize(width, height);
	        
	    
	}
	
	// ----------------------------------------------------------------------------
	public double getModelValue()
	{
	    return getModelValueFromSliderValue(_slider.getValue());
	}
	// ----------------------------------------------------------------------------
	
	  public void setModelValue(double modelValue)
	    {
	        int sliderValue = getSliderValueFromModelValue(modelValue);
	        
	        _slider.setValue(sliderValue);
	        
	        updateValueLabel();
	        
	    }
		
	    
//	  ----------------------------------------------------------------------------
	    public void addChangeListener(ChangeListener listener)
	    {
	        
	        _slider.addChangeListener(listener);
	        
	        return;
	    }
//	  ----------------------------------------------------------------------------
	    
	
	private int getSliderValueFromModelValue(double modelValue)
	{
	    double modelRange = _maxModelValue - _minModelValue;
	    double sliderRange = _sliderMax - _sliderMin;
	    
	    double modelDiff = modelValue - _minModelValue;
	    double modelFraction = modelDiff / modelRange;
	    
	    double sliderDoubleValue =  _sliderMin + (modelFraction * sliderRange);  
	    
	    return (int) Math.round(sliderDoubleValue);
	}
	
//	 ----------------------------------------------------------------------------
	

	private double getModelValueFromSliderValue(int sliderValue)
	{
	    
	    double modelRange = _maxModelValue - _minModelValue;
	    double sliderRange = _sliderMax - _sliderMin;
	    
	    double sliderDiff = sliderValue - _sliderMin;
	    double sliderFraction = sliderDiff / sliderRange;
	    
	    double modelValue =  _minModelValue + (sliderFraction * modelRange);  
	    
	    modelValue = MathHelper.roundToNDecimalPlaces(modelValue, 2);
	        
	    return modelValue;
	}
	// ----------------------------------------------------------------------------
	
	private String getSliderValueLabelString(double modelValue)
	{
	    String modelValueString = _numberFormat.format(modelValue);   
	    modelValueString = padToNChars(modelValueString, 6);
	    
	    String maxModelValueString = _numberFormat.format(_maxModelValue); 
	    maxModelValueString = padToNChars(maxModelValueString, 6);
		
	    String stringValue = modelValueString + "| max = " + maxModelValueString;
	    return stringValue;
	}

	// ----------------------------------------------------------------------------

	private String padToNChars(String originalString, int charCount)
	{
	    String newString = originalString;
	    int length = originalString.length() ;
	    if (length < charCount)
	    {
	        int diff = charCount - length;
	        StringBuffer buffer = new StringBuffer(originalString);
	        for (int i = 0; i < diff; i++)
	        {
	            buffer.insert(0, ' ');
	        }
	        newString = buffer.toString();
	    }
	    
	    return newString;
	}
	// ----------------------------------------------------------------------------
	/**
     * @param slider The slider to set.
     */
    private void setSlider(JSlider slider)
    {
        _slider = slider;
    }
    
//  ----------------------------------------------------------------------------
    
    /**
     * @return Returns the slider.
     */
    private JSlider getSlider()
    {
        return _slider;
    }
    
//  ----------------------------------------------------------------------------
    
    /**
     * @param sliderLabel The sliderLabel to set.
     */
    private void setLabel(JLabel label)
    {
        _label = label;
    }
    
//  ----------------------------------------------------------------------------
    
    /**
     * @return Returns the sliderLabel.
     */
    private JLabel getLabel()
    {
        return _label;
    }
    
//  ----------------------------------------------------------------------------
    
    /**
     * @param sliderValueLabel The sliderValueLabel to set.
     */
    private void setValueLabel(JLabel valueLabel)
    {
        _valueLabel = valueLabel;
    }
//  ----------------------------------------------------------------------------
    private void updateValueLabel()
    {
        String labelString = getSliderValueLabelString(getModelValue());
        _valueLabel.setText(labelString);
    }
//  ----------------------------------------------------------------------------
	
    /**
     * @return Returns the sliderValueLabel.
     */
    private JLabel getValueLabel()
    {
        return _valueLabel;
    }
//  ----------------------------------------------------------------------------
    
  
    
    /*
    private class LabeledSliderChangeListener implements ChangeListener
    {
        public void stateChanged(ChangeEvent e)
        {
            double newValue = getModelValueFromSliderValue(getSlider().getValue());
            _valueLabel.setText(getSliderValueLabelString(newValue));
                      
            _streamModel.setLztwc(newValue);
            notifyListeners("LZTWC changed");
        }
    }
    */
//	  ----------------------------------------------------------------------------


}
