package ohd.hseb.util.gui;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.util.EventListener;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.gui.ComponentHelper;

public class LabeledTextField extends JPanel
{
    private JLabel _label = new JLabel();
    private JTextField _textField = new JTextField();
    private GridBagConstraints _gbLabelConstraints = new GridBagConstraints();
    private GridBagConstraints _gbTFConstraints = new GridBagConstraints();

    public LabeledTextField()
    {
//        setPreferredSize( new Dimension( 240, 15 ) );
        setupConstraints();
        setupComponents();
//        this.setBackground( Color.YELLOW  );
    }
    
    public LabeledTextField( String labelText, String textFieldText, int textFieldWidthInColumns )
    {
        this();
        setLabel( labelText );
        setTextField( textFieldText );
        setTextFieldWidth( textFieldWidthInColumns );
    }
    
    public LabeledTextField( String labelText, String textFieldText, String toolTipText, int textFieldWidthInColumns )
    {
        this();
        setLabel( labelText );
        setTextField( textFieldText );
        setTextFieldWidth( textFieldWidthInColumns );
        setToolTipText( toolTipText );
        _label.setToolTipText( toolTipText );
        _textField.setToolTipText( toolTipText);
    }

    private void setupComponents()
    {
//        this.setLayout( new FlowLayout( FlowLayout.LEFT ) );
        setLayout( new GridBagLayout() );;
//        _label.setPreferredSize( new Dimension( 120, 15 ) );
//        _label.setMaximumSize( new Dimension( 100, 15 )  );
//        _textField.setPreferredSize( new Dimension( 120, 15 ) );
//        _textField.setMaximumSize( new Dimension( 100, 15 )  );
//                                                                               X,   Y,  #Col, #Row
        ComponentHelper.addPanelComponent( this, _label, _gbLabelConstraints,    0,   0,    1,    1, GridBagConstraints.VERTICAL );
        ComponentHelper.addPanelComponent( this, _textField, _gbTFConstraints,   1,   0,    1,    1, GridBagConstraints.VERTICAL );
//        this.add( _label );
//        this.add( _textField );

    }
    
    public String toString()
    {
        return "Label = " + _label.getText() + " | TextField = " + _textField.getText();
    }
    
    private void setupConstraints()
    {
        _gbLabelConstraints.anchor = GridBagConstraints.EAST;
        _gbLabelConstraints.weightx = 1;
        _gbLabelConstraints.weighty = 0;
        
        _gbTFConstraints.anchor = GridBagConstraints.WEST;
        _gbTFConstraints.weightx = 1;
        _gbTFConstraints.weighty = 0;
    }

    public void setLabel( String labelString )
    {
        _label.setText( labelString );
    }
    
    public String getLabelText()
    {
        return _label.getText();
    }
    
    public void setTextField( String textFieldText )
    {
        _textField.setText( textFieldText );
    }

    public void setTextField( double textFieldNumber )
    {
        setNumberField( textFieldNumber );
    }
    
    public void setTextField( int textFieldNumber )
    {
        setNumberField( textFieldNumber );
    }
    
    public void setTextField( short textFieldNumber )
    {
        setNumberField( textFieldNumber );
    }
    
    public void setTextFieldDate( long dateString )
    {
        _textField.setText( DbTimeHelper.getDateStringFromLongTime( dateString ) );
    }
    
    private void setNumberField( double doubleValue )
    {
        if ( DbTable.isNull( doubleValue ) )
        {
            _textField.setText( "" );
        }
        else
        {
            _textField.setText( Double.toString( doubleValue ) );
        }
    }
    
    private void setNumberField( short shortValue )
    {
        if ( DbTable.isNull( shortValue ) )
        {
            _textField.setText( "" );
        }
        else
        {
            _textField.setText( Short.toString( shortValue ) );
        }
    }

    private void setNumberField( int doubleValue )
    {
        if ( DbTable.isNull( doubleValue ) )
        {
            _textField.setText( "" );
        }
        else
        {
            _textField.setText( Integer.toString( doubleValue ) );
        }
    }


    public String getTextFieldText()
    {
        return _textField.getText();
    }
    
    public void setTextFieldWidth( int columns )
    {
        _textField.setColumns( columns );
    }
    
    public void setToolTip( String toolTipText )
    {
        _label.setToolTipText( toolTipText );
        _textField.setToolTipText( toolTipText );
    }
    
    public void enableTextField( boolean enable )
    {
        _textField.setEnabled( enable );
    }
    
    public void enableLabel( boolean enable )
    {
        _label.setEnabled( enable );
    }
    
    public void setEditTextField( boolean editable )
    {
        _textField.setEditable( editable );
    }
    
    public void enableLabeledTextField( boolean enable )
    {
        enableTextField( enable );
        enableLabel( enable );
    }
    
    public void addTextFieldKeyListener( KeyListener keyListener )
    {
        _textField.addKeyListener( keyListener );
    }
    
    public void addTextFieldMouseListener( MouseListener mouseListener )
    {
        _textField.addMouseListener( mouseListener );
    }
    
    public void setLabelPreferredSize( Dimension dimension )
    {
        _label.setPreferredSize( dimension );
    }
    
    public void setTextFieldPreferredSize( Dimension dimension )
    {
        _textField.setPreferredSize( dimension );
    }
    

}
