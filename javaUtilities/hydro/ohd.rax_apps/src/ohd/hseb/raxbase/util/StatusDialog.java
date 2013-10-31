package ohd.hseb.raxbase.util;

import java.awt.GridLayout;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class StatusDialog extends JDialog
{
    private JLabel _label = new JLabel();
    
    public StatusDialog( String message )
    {
        super( (JFrame) null, true );
        _label.setText( message );
        getContentPane().setLayout( new GridLayout( 1, 1 ) );
        this.add( _label );
        setAlwaysOnTop( true );
        pack();
    }

    public void setMessage( String message )
    {
        _label.setText( message );
    }
    
    public String getMessage()
    {
        return _label.getText();
    }
    public JLabel getLabel()
    {
        return _label;
    }
    
    public void firePropListener()
    {
        firePropertyChange( "DONE", null, null );
    }
}
