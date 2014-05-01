package org.rzo.yajsw.ws;

import com.jeta.forms.components.border.TitledBorderBottom;
import com.jeta.forms.components.border.TitledBorderSide;
import com.jeta.forms.components.separator.TitledSeparator;
import com.jeta.open.i18n.I18NUtils;
import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;
import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;


public class WSForm extends JPanel
{
   JTextArea _LOG_AREA = new JTextArea();
   TitledBorderBottom _titledborderbottom1 = new TitledBorderBottom();
   JRadioButton _INSTALL_OPTION = new JRadioButton();
   ButtonGroup _buttongroup1 = new ButtonGroup();
   JRadioButton _CONSOLE_OPTION = new JRadioButton();
   JLabel _APPLICATION = new JLabel();
   TitledBorderSide _titledborderside1 = new TitledBorderSide();
   JCheckBox _START_OPTION = new JCheckBox();
   JTextField _INSTALL_FOLDER = new JTextField();
   JCheckBox _TRAY_ICON_OPTION = new JCheckBox();
   JButton _GO_BUTTON = new JButton();
   JButton _CANCEL_BUTTON = new JButton();
   JLabel _STATE = new JLabel();
   JLabel _SPEED = new JLabel();
   JButton _SELECT_FOLDER_BUTTON = new JButton();
   JButton _SHOW_CONF_BUTTON = new JButton();
   TitledBorderSide _titledborderside2 = new TitledBorderSide();

   /**
    * Default constructor
    */
   public WSForm()
   {
      initializePanel();
   }

   /**
    * Main method for panel
    */
   public static void main(String[] args)
   {
      JFrame frame = new JFrame();
      frame.setSize(600, 400);
      frame.setLocation(100, 100);
      frame.getContentPane().add(new WSForm());
      frame.setVisible(true);

      frame.addWindowListener( new WindowAdapter()
      {
         public void windowClosing( WindowEvent evt )
         {
            System.exit(0);
         }
      });
   }

   /**
    * Adds fill components to empty cells in the first row and first column of the grid.
    * This ensures that the grid spacing will be the same as shown in the designer.
    * @param cols an array of column indices in the first row where fill components should be added.
    * @param rows an array of row indices in the first column where fill components should be added.
    */
   void addFillComponents( Container panel, int[] cols, int[] rows )
   {
      Dimension filler = new Dimension(10,10);

      boolean filled_cell_11 = false;
      CellConstraints cc = new CellConstraints();
      if ( cols.length > 0 && rows.length > 0 )
      {
         if ( cols[0] == 1 && rows[0] == 1 )
         {
            /** add a rigid area  */
            panel.add( Box.createRigidArea( filler ), cc.xy(1,1) );
            filled_cell_11 = true;
         }
      }

      for( int index = 0; index < cols.length; index++ )
      {
         if ( cols[index] == 1 && filled_cell_11 )
         {
            continue;
         }
         panel.add( Box.createRigidArea( filler ), cc.xy(cols[index],1) );
      }

      for( int index = 0; index < rows.length; index++ )
      {
         if ( rows[index] == 1 && filled_cell_11 )
         {
            continue;
         }
         panel.add( Box.createRigidArea( filler ), cc.xy(1,rows[index]) );
      }

   }

   /**
    * Helper method to load an image file from the CLASSPATH
    * @param imageName the package and name of the file to load relative to the CLASSPATH
    * @return an ImageIcon instance with the specified image file
    * @throws IllegalArgumentException if the image resource cannot be loaded.
    */
   public ImageIcon loadImage( String imageName )
   {
      try
      {
         ClassLoader classloader = getClass().getClassLoader();
         java.net.URL url = classloader.getResource( imageName );
         if ( url != null )
         {
            ImageIcon icon = new ImageIcon( url );
            return icon;
         }
      }
      catch( Exception e )
      {
         e.printStackTrace();
      }
      throw new IllegalArgumentException( "Unable to load image: " + imageName );
   }

   /**
    * Method for recalculating the component orientation for 
    * right-to-left Locales.
    * @param orientation the component orientation to be applied
    */
   public void applyComponentOrientation( ComponentOrientation orientation )
   {
      // Not yet implemented...
      // I18NUtils.applyComponentOrientation(this, orientation);
      super.applyComponentOrientation(orientation);
   }

   public JPanel createPanel()
   {
      JPanel jpanel1 = new JPanel();
      FormLayout formlayout1 = new FormLayout("FILL:DEFAULT:NONE,FILL:8DLU:NONE,FILL:84PX:NONE,FILL:DEFAULT:NONE,FILL:97PX:NONE,FILL:DEFAULT:NONE,FILL:109PX:NONE,FILL:DEFAULT:NONE,FILL:95PX:NONE,FILL:25PX:NONE,FILL:8DLU:NONE,FILL:8DLU:NONE,FILL:DEFAULT:NONE","CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,FILL:174PX:NONE,CENTER:4DLU:NONE,CENTER:DEFAULT:NONE");
      CellConstraints cc = new CellConstraints();
      jpanel1.setLayout(formlayout1);

      _LOG_AREA.setName("LOG_AREA");
      JScrollPane jscrollpane1 = new JScrollPane();
      jscrollpane1.setViewportView(_LOG_AREA);
      jscrollpane1.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      jscrollpane1.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      jpanel1.add(jscrollpane1,cc.xywh(3,14,9,1));

      TitledSeparator titledseparator1 = new TitledSeparator();
      titledseparator1.setText("Log");
      jpanel1.add(titledseparator1,cc.xywh(3,13,9,1));

      jpanel1.add(_titledborderbottom1,cc.xywh(3,15,9,1));

      JLabel jlabel1 = new JLabel();
      jlabel1.setText("Action");
      jpanel1.add(jlabel1,cc.xy(3,8));

      _INSTALL_OPTION.setActionCommand("Install Service");
      _INSTALL_OPTION.setName("INSTALL_OPTION");
      _INSTALL_OPTION.setText("Install Service");
      _buttongroup1.add(_INSTALL_OPTION);
      jpanel1.add(_INSTALL_OPTION,cc.xy(5,8));

      _CONSOLE_OPTION.setActionCommand("Run Console");
      _CONSOLE_OPTION.setName("CONSOLE_OPTION");
      _CONSOLE_OPTION.setText("Run Console");
      _buttongroup1.add(_CONSOLE_OPTION);
      jpanel1.add(_CONSOLE_OPTION,cc.xy(5,9));

      JLabel jlabel2 = new JLabel();
      jlabel2.setText("Application");
      jpanel1.add(jlabel2,cc.xy(3,4));

      JLabel jlabel3 = new JLabel();
      jlabel3.setText("Install Folder");
      jpanel1.add(jlabel3,cc.xy(3,6));

      _APPLICATION.setName("APPLICATION");
      _APPLICATION.setText("???");
      jpanel1.add(_APPLICATION,cc.xywh(5,4,6,1));

      jpanel1.add(_titledborderside1,cc.xywh(2,13,1,3));

      _START_OPTION.setActionCommand("& Start");
      _START_OPTION.setName("START_OPTION");
      _START_OPTION.setText("Start Service");
      jpanel1.add(_START_OPTION,cc.xy(7,8));

      _INSTALL_FOLDER.setName("INSTALL_FOLDER");
      jpanel1.add(_INSTALL_FOLDER,cc.xywh(5,6,6,1));

      _TRAY_ICON_OPTION.setActionCommand("& Tray Icon");
      _TRAY_ICON_OPTION.setName("TRAY_ICON_OPTION");
      _TRAY_ICON_OPTION.setText("Tray Icon");
      jpanel1.add(_TRAY_ICON_OPTION,cc.xy(9,8));

      jpanel1.add(createPanel1(),cc.xywh(2,11,11,1));
      _SELECT_FOLDER_BUTTON.setActionCommand("...");
      _SELECT_FOLDER_BUTTON.setName("SELECT_FOLDER_BUTTON");
      _SELECT_FOLDER_BUTTON.setText("...");
      jpanel1.add(_SELECT_FOLDER_BUTTON,cc.xy(12,6));

      _SHOW_CONF_BUTTON.setActionCommand("...");
      _SHOW_CONF_BUTTON.setName("SHOW_CONF_BUTTON");
      _SHOW_CONF_BUTTON.setText("...");
      jpanel1.add(_SHOW_CONF_BUTTON,cc.xy(12,4));

      JLabel jlabel4 = new JLabel();
      jlabel4.setFont(new Font("Tahoma",Font.BOLD,16));
      jlabel4.setText("YAJSW - Java Web Start Booter");
      jlabel4.setHorizontalAlignment(JLabel.CENTER);
      jpanel1.add(jlabel4,cc.xywh(2,2,11,1));

      _titledborderside2.setOrientation(TitledBorderSide.RIGHT);
      jpanel1.add(_titledborderside2,cc.xywh(12,13,1,3));

      addFillComponents(jpanel1,new int[]{ 1,2,3,4,5,6,7,8,9,10,11,12,13 },new int[]{ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 });
      return jpanel1;
   }

   public JPanel createPanel1()
   {
      JPanel jpanel1 = new JPanel();
      FormLayout formlayout1 = new FormLayout("FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:235PX:NONE,FILL:86PX:NONE,FILL:DEFAULT:NONE,FILL:DEFAULT:NONE","CENTER:DEFAULT:NONE");
      CellConstraints cc = new CellConstraints();
      jpanel1.setLayout(formlayout1);

      _GO_BUTTON.setActionCommand("Start");
      _GO_BUTTON.setName("GO_BUTTON");
      _GO_BUTTON.setText("Continue");
      jpanel1.add(_GO_BUTTON,cc.xy(2,1));

      _CANCEL_BUTTON.setActionCommand("Cancel");
      _CANCEL_BUTTON.setName("CANCEL_BUTTON");
      _CANCEL_BUTTON.setText("Close");
      jpanel1.add(_CANCEL_BUTTON,cc.xy(5,1));

      _STATE.setFont(new Font("Tahoma",Font.BOLD,12));
      _STATE.setName("STATE");
      _STATE.setText("Starting");
      _STATE.setHorizontalAlignment(JLabel.CENTER);
      jpanel1.add(_STATE,cc.xy(3,1));

      _SPEED.setName("SPEED");
      jpanel1.add(_SPEED,cc.xy(4,1));

      addFillComponents(jpanel1,new int[]{ 1,6 },new int[]{ 1 });
      return jpanel1;
   }

   /**
    * Initializer
    */
   protected void initializePanel()
   {
      setLayout(new BorderLayout());
      add(createPanel(), BorderLayout.CENTER);
   }


}
