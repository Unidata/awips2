package ohd.hseb.util.gui;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelListener;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import ohd.hseb.util.gui.ComponentHelper;

public class LabeledComboBox extends JPanel
{
    private JLabel _label = new JLabel();
    private JComboBox _comboBox = new JComboBox();
    private GridBagConstraints _gbLabelConstraints = new GridBagConstraints();
    private GridBagConstraints _gbTFConstraints = new GridBagConstraints();

    public LabeledComboBox()
    {
        setupConstraints();
        setupComponents();
    }
    
    public LabeledComboBox( String labelText )
    {
        this();
        setLabel( labelText );
    }
    
    public LabeledComboBox( String labelText, String[] comboBoxStringArray )
    {
        this();
        setLabel( labelText );
        setComboBox( comboBoxStringArray );
    }

    public LabeledComboBox( String labelText, String tooltip )
    {
        this();
        setLabel( labelText );
        _comboBox.setToolTipText( tooltip );
    }
    
    public LabeledComboBox( String labelText, List comboBoxStringList )
    {
        this();
        setLabel( labelText );
        setComboBox( comboBoxStringList );
    }
    
    public LabeledComboBox( String labelText, String[] comboBoxStringArray, String toolTipText )
    {
        this( labelText, comboBoxStringArray );
        setToolTipText( toolTipText );
    }

    public LabeledComboBox( String labelText, List comboBoxStringList, String toolTipText )
    {
        this( labelText, comboBoxStringList );
        setToolTipText( toolTipText );
    }

    private void setupComponents()
    {
        this.setLayout( new GridBagLayout() );
//        _label.setPreferredSize( new Dimension( 150, 15 ) );
//        _comboBox.setPreferredSize( new Dimension( 150, 15 ) );
//                                                                               X,   Y,  #Col, #Row
        ComponentHelper.addPanelComponent( this, _label, _gbLabelConstraints,    0,   0,    1,    1, GridBagConstraints.VERTICAL );
        ComponentHelper.addPanelComponent( this, _comboBox, _gbTFConstraints,    1,   0,    1,    1, GridBagConstraints.VERTICAL );
    }
    
    public String toString()
    {
        return "Label = " + _label.getText() + " | ComboBox = " + (String) _comboBox.getSelectedItem();
    }
    
    private void setupConstraints()
    {
        _gbLabelConstraints.anchor = GridBagConstraints.EAST;
        _gbLabelConstraints.weightx = 1;
        _gbLabelConstraints.weighty = 1;
        
        _gbTFConstraints.anchor = GridBagConstraints.WEST;
        _gbTFConstraints.weightx = 1;
        _gbTFConstraints.weighty = 1;
    }

    public void setLabelPreferredSize( Dimension dimension )
    {
        _label.setPreferredSize( dimension );
    }
    
    public void setComboBoxPreferredSize( Dimension dimension )
    {
        _comboBox.setPreferredSize( dimension );
    }
    
    public void setLabel( String labelString )
    {
        _label.setText( labelString );
    }
    
    public String getLabel()
    {
        return _label.getText();
    }
    
    public void setComboBox( JComboBox comboBox )
    {
        _comboBox = comboBox;
    }
    
    public void setComboBoxFromStringArray( Object[] comboBoxStringArray )
    {
        String comboBoxString = null;
        
        for ( int i = 0; i < comboBoxStringArray.length;i++ )
        {
            comboBoxString = (String) comboBoxStringArray[ i ];
            _comboBox.addItem( comboBoxString );
        }
    }

    public void setComboBox( String[] comboBoxStringArray )
    {
        for ( int i = 0; i < comboBoxStringArray.length; i++ )
        {
            _comboBox.addItem( comboBoxStringArray[ i ] );
        }
    }
    
    public void setComboBox( List comboBoxStringList )
    {
        String comboBoxString = null;
        _comboBox.removeAllItems();
        
        for ( int i = 0; i < comboBoxStringList.size(); i++ )
        {
            comboBoxString = (String) comboBoxStringList.get( i );
            _comboBox.addItem( comboBoxString );
        }
    }

    public JComboBox getComboBox()
    {
        return _comboBox;
    }
    
    public String getSelectedCBItem()
    {
        return ( (String) _comboBox.getSelectedItem() );
    }
    
    public void setToolTip( String toolTipText )
    {
        _label.setToolTipText( toolTipText );
    }
    
    public void enableComboBox( boolean enable )
    {
        _comboBox.setEnabled( enable );
    }
    
    public void enableLabel( boolean enable )
    {
        _label.setEnabled( enable );
    }
    
    public void setEditComboBox( boolean editable )
    {
        _comboBox.setEditable( editable );
    }
    
    public void setSelectedItem( Object object )
    {
        _comboBox.setSelectedItem( object );
    }
    
    public void setSelectedIndex( int index )
    {
        _comboBox.setSelectedIndex( index );
    }
    
    public int getSelectedIndex()
    {
        return _comboBox.getSelectedIndex();
    }
    
    public int getItemCount()
    {
        return _comboBox.getItemCount();
    }
    
    public Object getSelectedItem()
    {
        return _comboBox.getSelectedItem();
    }
    
    public void enableLabeledTextField( boolean enable )
    {
        enableComboBox( enable );
        enableLabel( enable );
    }
    
    public void setLabelFont( Font font )
    {
        _label.setFont( font );
    }
    
    public void setComboBoxFont( Font font )
    {
        _comboBox.setFont( font );
    }
    
    public void addComboBoxActionListener( ActionListener actionListener )
    {
        _comboBox.addActionListener( actionListener );
    }
    
    public void addComboBoxWheelListener( MouseWheelListener wheelListener )
    {
        _comboBox.addMouseWheelListener( wheelListener );
    }
    
    public void removeComboBoxActionListener( ActionListener actionListener )
    {
        _comboBox.removeActionListener( actionListener );
    }
}
