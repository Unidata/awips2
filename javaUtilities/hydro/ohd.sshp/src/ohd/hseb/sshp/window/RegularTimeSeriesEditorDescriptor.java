/*
 * Created on Apr 19, 2004
 *
 * Filename : RegularTimeSeriesHelper.java
 * Author   : Gautam Sood
 * Last Revision Date : Apr 19, 2004
 *  
 */

package ohd.hseb.sshp.window;

import java.awt.Frame;
import ohd.hseb.measurement.RegularTimeSeriesHolder;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.util.TimeHelper;

public class RegularTimeSeriesEditorDescriptor
{
	private Frame _owner = null;
	private RegularTimeSeriesHolder _regularTimeSeriesHolder = null;
	private String _titleLabelString = "Timeseries Editor";
	private String _valueLabelString = null;
	private String _textFieldFormatString = "0.00";
	private boolean _isEditable = true;
	private boolean _isModal = true;
	private ValueMapper _valueMapper = null;
	private String _mappedValueLabelString = null;
	private long _initialSearchTime = 0;
	private double _missingValue = -9999.00;
	private double _minValue = 0.0;
	private double _maxValue = 20.0;
	
	
	public RegularTimeSeriesEditorDescriptor( Frame owner,
                                              RegularTimeSeriesHolder regularTimeSeriesHolder,
									          String titleLabelString,
                                              String valueLabelString,
                                              String textFieldFormatString,
                                              boolean isEditable,
									          boolean isModal,
                                              ValueMapper valueMapper, 
                                              String mappedValueLabelString,
                                              long initialSearchTime,
                                              double missingValue,
                                              double minValue,
                                              double maxValue )
	{
		_owner = owner;
		_regularTimeSeriesHolder = regularTimeSeriesHolder;
		_titleLabelString = titleLabelString;
		_valueLabelString = valueLabelString;
		_textFieldFormatString = textFieldFormatString;
		_isEditable = isEditable;
		_isModal = isModal;
		_valueMapper = valueMapper;
		_mappedValueLabelString = mappedValueLabelString;
		_missingValue = missingValue;
		_minValue = minValue;
		_maxValue = maxValue;
		setInitialSearchTime( getRoundedTime( initialSearchTime ) );
	}

	public RegularTimeSeriesEditorDescriptor( Frame owner,
                                              RegularTimeSeriesHolder regularTimeSeriesHolder,
                                              String valueLabelString )
	{
		_owner = owner;
		_regularTimeSeriesHolder = regularTimeSeriesHolder;
		_valueLabelString = valueLabelString;
	}
	
	public long getRoundedTime( long initialSearchTime )
	{
		return TimeHelper.truncateTimeInMillisToNearestHour( initialSearchTime, 1 );
	}
	
	public void setOwner( Frame owner )
	{
		_owner = owner;
	}

	public Frame getOwner()
	{
		return _owner;
	}

	public void setRegularTimeSeriesHolder( RegularTimeSeriesHolder regularTimeSeriesHolder )
	{
		_regularTimeSeriesHolder = regularTimeSeriesHolder;
	}

	public RegularTimeSeriesHolder getRegularTimeSeriesHolder()
	{
		return _regularTimeSeriesHolder;
	}

	public void setTitleLabelString( String titleLabelString )
	{
		_titleLabelString = titleLabelString;
	}

	public String getTitleLabelString()
	{
		return _titleLabelString;
	}

	public void setValueLabelString( String valueLabelString )
	{
		_valueLabelString = valueLabelString;
	}

	public String getValueLabelString()
	{
		return _valueLabelString;
	}

	public void setTextFieldFormatString( String textFieldFormatString )
	{
		_textFieldFormatString = textFieldFormatString;
	}

	public String getTextFieldFormatString()
	{
		return _textFieldFormatString;
	}

	public void setEditable( boolean isEditable )
	{
		_isEditable = isEditable;
	}

	public boolean isEditable()
	{
		return _isEditable;
	}

	public void setModal( boolean isModal )
	{
		_isModal = isModal;
	}

	public boolean isModal()
	{
		return _isModal;
	}

	public void setValueMapper( ValueMapper valueMapper )
	{
		_valueMapper = valueMapper;
	}

	public ValueMapper getValueMapper()
	{
		return _valueMapper;
	}

	public void setMappedValueLabelString( String mappedValueLabelString )
	{
		_mappedValueLabelString = mappedValueLabelString;
	}

	public String getMappedValueLabelString()
	{
		return _mappedValueLabelString;
	}

	public void setInitialSearchTime( long initialSearchTime )
	{
		_initialSearchTime = getRoundedTime( initialSearchTime );
	}

	public long getInitialSearchTime()
	{
		return _initialSearchTime;
	}

	public void setMissingValue( double missingValue )
	{
		_missingValue = missingValue;
	}

	public double getMissingValue()
	{
		return _missingValue;
	}

	public void setMinValue( double minValue )
	{
		_minValue = minValue;
	}

	public double getMinValue()
	{
		return _minValue;
	}

	public void setMaxValue( double maxValue )
	{
		_maxValue = maxValue;
	}

	public double getMaxValue()
	{
		return _maxValue;
	}
}
