package org.rzo.yajsw.srvmgr.client;

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
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;


public class ServicesForm extends JPanel
{
   JButton _ADD_HOSTS_BUTTON = new JButton();
   JButton _REMOVE_HOSTS_BUTTON = new JButton();
   JButton _ADD_HIDDEN_BUTTON = new JButton();
   JButton _REMOVE_HIDDEN_BUTTON = new JButton();
   JTable _HIDDEN_TABLE = new JTable();
   JButton _START_BUTTON = new JButton();
   JButton _STOP_BUTTON = new JButton();
   JButton _INSTALL_BUTTON = new JButton();
   JButton _UNINSTALL_BUTTON = new JButton();
   JButton _RELOAD_CONSOLE = new JButton();
   JTable _HOSTS_TABLE = new JTable();
   JButton _DELETE_HOST_BUTTON = new JButton();
   JButton _NEW_HOST_BUTTON = new JButton();
   JTable _SERVICES_TABLE = new JTable();

   /**
    * Default constructor
    */
   public ServicesForm()
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
      frame.getContentPane().add(new ServicesForm());
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
      FormLayout formlayout1 = new FormLayout("FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:214PX:NONE,FILL:4DLU:NONE,FILL:56PX:NONE,FILL:4DLU:NONE,FILL:PREF:NONE,FILL:DEFAULT:NONE","CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,FILL:PREF:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:60PX:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:66PX:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:95PX:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:138PX:NONE,CENTER:DEFAULT:NONE");
      CellConstraints cc = new CellConstraints();
      jpanel1.setLayout(formlayout1);

      _ADD_HOSTS_BUTTON.setActionCommand("+");
      _ADD_HOSTS_BUTTON.setName("ADD_HOSTS_BUTTON");
      _ADD_HOSTS_BUTTON.setText(">>");
      _ADD_HOSTS_BUTTON.setToolTipText("Add Services from Host");
      _ADD_HOSTS_BUTTON.setHorizontalTextPosition(JButton.CENTER);
      jpanel1.add(_ADD_HOSTS_BUTTON,cc.xy(8,13));

      _REMOVE_HOSTS_BUTTON.setActionCommand("-");
      _REMOVE_HOSTS_BUTTON.setName("REMOVE_HOSTS_BUTTON");
      _REMOVE_HOSTS_BUTTON.setText("<<");
      jpanel1.add(_REMOVE_HOSTS_BUTTON,cc.xy(8,15));

      _ADD_HIDDEN_BUTTON.setActionCommand("+");
      _ADD_HIDDEN_BUTTON.setName("ADD_HIDDEN_BUTTON");
      _ADD_HIDDEN_BUTTON.setText("<<");
      jpanel1.add(_ADD_HIDDEN_BUTTON,cc.xy(8,21));

      _REMOVE_HIDDEN_BUTTON.setActionCommand("-");
      _REMOVE_HIDDEN_BUTTON.setName("REMOVE_HIDDEN_BUTTON");
      _REMOVE_HIDDEN_BUTTON.setText(">>");
      jpanel1.add(_REMOVE_HIDDEN_BUTTON,cc.xy(8,23));

      JLabel jlabel1 = new JLabel();
      jlabel1.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel1.setText("Hidden Services");
      jlabel1.setHorizontalAlignment(JLabel.CENTER);
      jpanel1.add(jlabel1,cc.xywh(2,18,5,1));

      _HIDDEN_TABLE.setName("HIDDEN_TABLE");
      JScrollPane jscrollpane1 = new JScrollPane();
      jscrollpane1.setViewportView(_HIDDEN_TABLE);
      jscrollpane1.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      jscrollpane1.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      jpanel1.add(jscrollpane1,cc.xywh(2,20,5,5));

      jpanel1.add(createPanel1(),new CellConstraints(10,4,1,3,CellConstraints.FILL,CellConstraints.FILL));
      JLabel jlabel2 = new JLabel();
      jlabel2.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel2.setText("Services");
      jlabel2.setHorizontalAlignment(JLabel.CENTER);
      jpanel1.add(jlabel2,cc.xy(10,2));

      _HOSTS_TABLE.setName("HOSTS_TABLE");
      JScrollPane jscrollpane2 = new JScrollPane();
      jscrollpane2.setViewportView(_HOSTS_TABLE);
      jscrollpane2.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      jscrollpane2.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      jpanel1.add(jscrollpane2,cc.xywh(2,4,5,13));

      JLabel jlabel3 = new JLabel();
      jlabel3.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel3.setText("Hosts");
      jlabel3.setHorizontalAlignment(JLabel.CENTER);
      jpanel1.add(jlabel3,cc.xywh(2,2,5,1));

      _DELETE_HOST_BUTTON.setActionCommand("-");
      _DELETE_HOST_BUTTON.setName("DELETE_HOST_BUTTON");
      _DELETE_HOST_BUTTON.setText("-");
      jpanel1.add(_DELETE_HOST_BUTTON,cc.xy(8,11));

      _NEW_HOST_BUTTON.setActionCommand("+");
      _NEW_HOST_BUTTON.setName("NEW_HOST_BUTTON");
      _NEW_HOST_BUTTON.setText("+");
      jpanel1.add(_NEW_HOST_BUTTON,cc.xy(8,9));

      _SERVICES_TABLE.setName("SERVICES_TABLE");
      JScrollPane jscrollpane3 = new JScrollPane();
      jscrollpane3.setViewportView(_SERVICES_TABLE);
      jscrollpane3.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      jscrollpane3.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      jpanel1.add(jscrollpane3,cc.xywh(10,8,1,17));

      addFillComponents(jpanel1,new int[]{ 1,2,3,4,5,6,7,8,9,10,11 },new int[]{ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25 });
      return jpanel1;
   }

   public JPanel createPanel1()
   {
      JPanel jpanel1 = new JPanel();
      FormLayout formlayout1 = new FormLayout("FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:39PX:NONE,FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:DEFAULT:NONE,FILL:68PX:NONE,FILL:DEFAULT:NONE,FILL:10PX:NONE","CENTER:DEFAULT:NONE");
      CellConstraints cc = new CellConstraints();
      jpanel1.setLayout(formlayout1);

      _START_BUTTON.setActionCommand("Start");
      _START_BUTTON.setName("START_BUTTON");
      _START_BUTTON.setText("Start");
      jpanel1.add(_START_BUTTON,cc.xy(1,1));

      _STOP_BUTTON.setActionCommand("Stop");
      _STOP_BUTTON.setName("STOP_BUTTON");
      _STOP_BUTTON.setText("Stop");
      jpanel1.add(_STOP_BUTTON,cc.xy(3,1));

      _INSTALL_BUTTON.setActionCommand("Install");
      _INSTALL_BUTTON.setName("INSTALL_BUTTON");
      _INSTALL_BUTTON.setText("Install");
      jpanel1.add(_INSTALL_BUTTON,cc.xy(5,1));

      _UNINSTALL_BUTTON.setActionCommand("Uninstall");
      _UNINSTALL_BUTTON.setName("UNINSTALL_BUTTON");
      _UNINSTALL_BUTTON.setText("Uninstall");
      jpanel1.add(_UNINSTALL_BUTTON,cc.xy(7,1));

      _RELOAD_CONSOLE.setActionCommand("ReloadConsoleApp");
      _RELOAD_CONSOLE.setName("RELOAD_CONSOLE");
      _RELOAD_CONSOLE.setText("ReloadConsoleApp");
      jpanel1.add(_RELOAD_CONSOLE,cc.xy(9,1));

      addFillComponents(jpanel1,new int[]{ 2,4,6,8,10 },new int[0]);
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
