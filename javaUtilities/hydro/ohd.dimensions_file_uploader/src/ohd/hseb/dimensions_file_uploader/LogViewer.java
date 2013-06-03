package ohd.hseb.dimensions_file_uploader;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.LabeledComboBox;

public class LogViewer extends JDialog
{
    private Container _dialogContentPane = getContentPane();
    
    private LabeledComboBox _logSelectionComboBox = new LabeledComboBox( "Select DR/DCS Log to View:" );
    
    private String _logFileListDir = "/awips/hydroapps/PVCSFileQueue/log/";
    
    private JTextArea _drInfoTextArea = new JTextArea();
    private JScrollPane _scrollPane = new JScrollPane( _drInfoTextArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
    private Map _drNumToLogFileMap = new HashMap();
    
    private LogSelectionCBListener _logSelectionCBListener = new LogSelectionCBListener();
    private JPanel _buttonPanel = new JPanel( new GridBagLayout() );
    private JButton _closeButton = new JButton( "Close" );
    
    public LogViewer( JFrame frame )
    {
        super( frame, "Log Viewer", true );
    }
    
    public void displayGUI()
    {
        initGUI();
    }
    
    private void initGUI()
    {
        setLayout( new GridBagLayout() );
        initSelectionComboBox();
        initButtonPanel();
        initFrameComponents();
        addListeners();
        setPreferredSize( new Dimension( 1100, 768 ) );
        pack();
        setVisible( true );
    }

    protected void initButtonPanel()
    {
//                                                                       X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _buttonPanel, _closeButton,   0,   0,    1,     1, 1, 1, GridBagConstraints.NONE );
    }

    private void initSelectionComboBox()
    {
        File filelistDir = new File( _logFileListDir );
        File[] filelistDirFileArray = filelistDir.listFiles( new LogFileFilter() );
        List drList = new ArrayList();

        for ( int i = 0; i < filelistDirFileArray.length; i++ )
        {
            File drDir = filelistDirFileArray[ i ];
            String fileName = getDRFromFileName( drDir.getName() );
            drList.add( fileName );
            _drNumToLogFileMap.put( fileName, drDir );
        }
        Collections.sort( drList );
        _logSelectionComboBox.setComboBox( drList );
    }
    
    private String getDRFromFileName( String fileName )
    {
        String drNum = null;
        
        drNum = fileName.substring( 30 );
        
        return drNum;
    }
    
    private void initFrameComponents()
    {
        _drInfoTextArea.setEditable( false );
//        _drInfoTextArea.setPreferredSize( new Dimension( 20, 50 ) );
        setLayout( new GridBagLayout() );
//                                                                                     X,   Y,  #Col,  #Row
        ComponentHelper.addFrameComponent( _dialogContentPane, _logSelectionComboBox,  0,   0,    4,     1, 1, 1, GridBagConstraints.HORIZONTAL );
        ComponentHelper.addFrameComponent( _dialogContentPane, _scrollPane,            0,   1,    4,     1, 1,10, GridBagConstraints.BOTH );
        ComponentHelper.addFrameComponent( _dialogContentPane, _buttonPanel,           0,   2,    4,     1, 1, 1, GridBagConstraints.BOTH );
    }
    
    private void refreshLog()
    {
        FileReader fileReader = null;
        BufferedReader bufferedReader = null;
        File logFile = (File) _drNumToLogFileMap.get( _logSelectionComboBox.getSelectedCBItem() );
        StringBuffer stringBuffer = new StringBuffer();
        String line = null;
        
        try
        {
            fileReader = new FileReader( logFile );
            bufferedReader = new BufferedReader( fileReader );

            line = bufferedReader.readLine();

            while ( line != null )
            {
                stringBuffer.append( line + "\n" );
                line = bufferedReader.readLine();
            }
            
            _drInfoTextArea.setText( stringBuffer.toString() );
        }
        catch( FileNotFoundException f )
        {

        }
        catch( IOException e )
        {

        }
        
    }
    
    private void addListeners()
    {
        WindowCloserListener windowCloser = new WindowCloserListener();

        _logSelectionComboBox.addComboBoxActionListener( _logSelectionCBListener );
        _closeButton.addActionListener( windowCloser );
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

    private void closeWindow()
    /********************
        Purpose: Exits the program gracefully 
     *********************/
    {
        this.dispose();
    }

    
    private class LogSelectionCBListener implements ActionListener
    {
        public void actionPerformed( ActionEvent e )
        {
            refreshLog();
        }
    }
    
    public class LogFileFilter implements FilenameFilter  
    {  
        public boolean accept( File dir, String name )
        {
            boolean valid = false;
            if ( ( name.startsWith( "upload_file_to_dimensions.log.DR" ) ) || ( name.startsWith( "upload_file_to_dimensions.log.DCS" ) ) )
            {
                valid = true;
            }
            return valid;
        }  
    }
    
    public static void main(String[] args) 
    {
        LogViewer logViewer = new LogViewer( new JFrame() );
//      dimensions_file_uploader.processAttributesFileDir();
        logViewer.initGUI();
    }

    
}
