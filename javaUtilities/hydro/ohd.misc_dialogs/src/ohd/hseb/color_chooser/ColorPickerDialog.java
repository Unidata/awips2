package ohd.hseb.color_chooser;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JPanel;

import ohd.hseb.util.gui.ComponentHelper;

public class ColorPickerDialog extends JDialog
{
    private ColorHolder _colorHolder = null;
//    private ColorSet _colorSet = null;
//    private int _indexValueOfDefaultColor = 0;
    private Container _frameContentPane = getContentPane();
    private JPanel _colorDialogPanel = new JPanel();
    private JColorChooser _jColorChooserDialog = new JColorChooser();

    private JPanel _buttonPanel = new JPanel();
    private JButton _okButton = new JButton( "OK" );
    private JButton _cancelButton = new JButton( "Cancel" );
    

/*
    public ColorPickerDialog( Frame owner, Color color, ColorSet colorSet )
    {
        super( owner, "Color Picker", true );
        
        _frameContentPane.setLayout( new GridBagLayout() );
        _colorDialogPanel.setLayout( new GridBagLayout() );

        _jColorChooserDialog.setColor( color );

        ComponentHelper.addFrameComponent( _frameContentPane, _colorDialogPanel, 1, 1, 1, 1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _colorDialogPanel, _jColorChooserDialog, 1, 1, 1, 1, GridBagConstraints.BOTH );
        setSize( 400, 600 );

    }
*/
/*    
    public ColorPickerDialog( JDialog owner, Color color, ColorSet colorSet )
    {
        super( owner, "Color Picker", true );
        _colorSet = colorSet;
        _defaultColor = color;
        initGUI();
        _indexValueOfDefaultColor = getIndexOfDefaultColorFromColorSet();
        
        addListeners();
    }
*/
    public ColorPickerDialog( JDialog owner, ColorHolder colorHolder )
    {
        super( owner, "Color Picker", true );
        _colorHolder = colorHolder;
        initGUI();
//        _indexValueOfDefaultColor = getIndexOfDefaultColorFromColorSet();
        
        addListeners();
    }
    
    
    private void initGUI()
    {
        _frameContentPane.setLayout( new GridBagLayout() );
        _colorDialogPanel.setLayout( new GridBagLayout() );
        _buttonPanel.setLayout( new GridBagLayout() );

        _jColorChooserDialog.setColor( _colorHolder.getColor() );

        setSize( 430, 400 );
        _buttonPanel.setPreferredSize( new Dimension( 30, 40 ) );
//                                                                                  X,   Y,  #Col,  #Row

        ComponentHelper.addFrameComponent( _frameContentPane, _colorDialogPanel,    1,   1,   1,      1, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _frameContentPane, _buttonPanel,         1,   2,   1,      3, GridBagConstraints.BOTH );

        ComponentHelper.addPanelComponent( _colorDialogPanel, _jColorChooserDialog, 1,   1,   1,      1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _buttonPanel, _okButton,                 1,   1,   1,      1, GridBagConstraints.BOTH );
        ComponentHelper.addPanelComponent( _buttonPanel, _cancelButton,             2,   1,   1,      1, GridBagConstraints.BOTH );
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _okButton.addActionListener( new OkButtonListener() );
        _cancelButton.addActionListener( windowCloser );
        
        
    }
    private class WindowCloserListener extends WindowAdapter implements ActionListener
    {
        public void windowClosing ( WindowEvent e )
        {
            closeWindow();
        }
        
        public void actionPerformed( ActionEvent e )
        {
            closeWindow();
        }
    }

/*    
    private int getIndexOfDefaultColorFromColorSet()
    {
        List colorList = _colorSet.getNamedColorList();
        Color extractedColor = null;
        int index = -1;
        
        for ( int i = 0; i < colorList.size(); i++ )
        {
            extractedColor = (Color) colorList.get( i );
            if ( extractedColor.equals( _defaultColor ) )
            {
                index = i;
            }
        }
        
        return index;
    }
*/
    
    private class OkButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            _colorHolder.setColor( _jColorChooserDialog.getColor() );
//            ColorPickerDialog.this.setVisible( false );
            closeWindow();
        }
    }

    /*    
    private class OkButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            List colorSetList = _colorSet.getNamedColorList();
            Color colorObject = null;
    
            colorSetList.set( _indexValueOfDefaultColor, _jColorChooserDialog.getColor() );

            closeWindow();
        }
    }


    private class OkButtonListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            List colorSetList = _colorSet.getNamedColorList();
            Color colorObject = null;
    
            colorSetList.set( _indexValueOfDefaultColor, _jColorChooserDialog.getColor() );

            closeWindow();
        }
    }
*/    
    
    private void closeWindow()
    /********************
    Purpose: Exit's the program gracefully 
    *********************/
    {
        this.dispose();
    }

 
    public static void main ( String[] args )
    {
//        ColorPickerDialog colorPicker = new ColorPickerDialog( new Frame(), Color.blue, true );
//        colorPicker.setVisible( true );
    }

}
