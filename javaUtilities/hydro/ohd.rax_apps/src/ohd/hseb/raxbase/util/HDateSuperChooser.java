package ohd.hseb.raxbase.util;

import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.BoxLayout;
import javax.swing.JButton;

import ohd.hseb.rfc.util.graphics.HDateChooser;
import ohd.hseb.rfc.util.graphics.HDateChooserOwner;
import ohd.hseb.rfc.util.graphics.HSwingFactory;

public class HDateSuperChooser extends HDateChooser
{
    protected final static int CLEAR_BUTTON = 2;
    protected boolean _clearDate = false;
    
    public HDateSuperChooser( boolean arg0 )
    {
        super( arg0 );
        JButton nullButton = new JButton( "Clear" );
        nullButton.setFont( _buttonFont);
        String[] buttonStringArray = {"OK", "Cancel", "Clear"};
        int[] groupsizes = {3};
        Insets theinsets = new Insets(5,5,5,0);
        
        GridBagConstraints constraints = HSwingFactory.returnGridBagConstraints(0,3,1,1,1,1,
                GridBagConstraints.WEST,GridBagConstraints.BOTH,theinsets,0,0);

        
        _buttons = HSwingFactory.createJButtons(buttonStringArray, _buttonFont, this);
        _buttonPanel.removeAll();
        _buttonPanel = HSwingFactory.createJButtonBox(_buttons, groupsizes, BoxLayout.X_AXIS);

        
        _fullPanel.add(_buttonPanel, constraints);

//        _buttonPanel.add( nullButton);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        _clearDate = false;
        super.actionPerformed( e );
        if ( e.getSource() == _buttons[CLEAR_BUTTON] )
        {
            int i;
            for ( i = 0; i < _owners.size(); i++ )
            {
                _clearDate = true;
                ( (HDateChooserOwner)(_owners.get( i ))).dateChosen( this );
            }
            setVisible(false);
        }
    }

    public boolean isClearDate()
    {
        return _clearDate;
    }
}
