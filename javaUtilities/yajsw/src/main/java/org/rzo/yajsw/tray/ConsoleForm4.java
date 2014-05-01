package org.rzo.yajsw.tray;

import java.awt.BorderLayout;
import java.awt.Color;
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
import javax.swing.JTextArea;
import javax.swing.JTextField;

import com.jeta.forms.components.border.TitledBorderBottom;
import com.jeta.forms.components.border.TitledBorderLabel;
import com.jeta.forms.components.border.TitledBorderSide;
import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;


public class ConsoleForm4 extends JPanel
{
   JTextArea _output = new JTextArea();
   JTextField _input = new JTextField();
   JButton _START_BUTTON = new JButton();
   JButton _STOP_BUTTON = new JButton();
   JButton _RESTART_BUTTON = new JButton();
   JButton _EXIT_WRAPPER_BUTTON = new JButton();
   JButton _THREAD_DUMP_BUTTON = new JButton();
   JLabel _appStopTime = new JLabel();
   JLabel _state = new JLabel();
   JLabel _wStartTime = new JLabel();
   JLabel _trigger = new JLabel();
   JButton _STOP_TIMER_BUTTON = new JButton();
   JLabel _appPid = new JLabel();
   JLabel _appStartTime = new JLabel();
   JLabel _wPid = new JLabel();
   TitledBorderLabel _titledborderlabel1 = new TitledBorderLabel();
   TitledBorderSide _titledborderside1 = new TitledBorderSide();
   TitledBorderSide _titledborderside2 = new TitledBorderSide();
   TitledBorderBottom _titledborderbottom1 = new TitledBorderBottom();
   TitledBorderLabel _titledborderlabel2 = new TitledBorderLabel();
   TitledBorderSide _titledborderside3 = new TitledBorderSide();
   TitledBorderSide _titledborderside4 = new TitledBorderSide();
   TitledBorderBottom _titledborderbottom2 = new TitledBorderBottom();
   JButton _jbutton1 = new JButton();
   JLabel _timer = new JLabel();
   JLabel _condition = new JLabel();
   JLabel _wrapperType = new JLabel();
   JLabel _cpu = new JLabel();
   JLabel _memory = new JLabel();
   JLabel _handles = new JLabel();
   JLabel _threads = new JLabel();
   JLabel _count = new JLabel();
   JLabel _exitCode = new JLabel();
   JButton _THREAD_DUMP_WRAPPER_BUTTON = new JButton();
   JButton _EXIT_TRAY_ICON_BUTTON = new JButton();
   JButton _START_OUTPUT_BUTTON = new JButton();
   JButton _PAUSE_OUTPUT_BUTTON = new JButton();
   JTextField __OUTPUT_FILTER = new JTextField();
   JButton _CLEAR_OUTPUT_BUTTON = new JButton();
   JButton _GC_BUTTON = new JButton();
   JButton _DUMP_HEAP_BUTTON = new JButton();

   /**
    * Default constructor
    */
   public ConsoleForm4()
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
      frame.getContentPane().add(new ConsoleForm4());
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
      FormLayout formlayout1 = new FormLayout("FILL:4DLU:NONE,FILL:4DLU:NONE,FILL:4DLU:NONE,LEFT:100PX:NONE,FILL:4DLU:NONE,RIGHT:100PX:NONE,FILL:4DLU:NONE,LEFT:100PX:NONE,FILL:4DLU:NONE,LEFT:100PX:NONE,FILL:4DLU:NONE,LEFT:100PX:NONE,FILL:4DLU:NONE,FILL:4DLU:NONE,FILL:4DLU:NONE,RIGHT:100PX:NONE,FILL:4DLU:NONE,FILL:100PX:NONE,FILL:4DLU:NONE,FILL:100PX:NONE,FILL:4DLU:NONE","CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,FILL:238PX:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,FILL:14DLU:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE,CENTER:DEFAULT:NONE,CENTER:4DLU:NONE,CENTER:DEFAULT:NONE,CENTER:DEFAULT:NONE,CENTER:4DLU:NONE,CENTER:4DLU:NONE,CENTER:DEFAULT:NONE,CENTER:2DLU:NONE");
      CellConstraints cc = new CellConstraints();
      jpanel1.setLayout(formlayout1);

      JLabel jlabel1 = new JLabel();
      jlabel1.setBackground(new Color(204,204,204));
      jlabel1.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel1.setOpaque(true);
      jlabel1.setText("Output");
      jpanel1.add(jlabel1,new CellConstraints(4,2,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _output.setName("output");
      JScrollPane jscrollpane1 = new JScrollPane();
      jscrollpane1.setViewportView(_output);
      jscrollpane1.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
      jscrollpane1.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
      jpanel1.add(jscrollpane1,cc.xywh(4,4,17,1));

      JLabel jlabel2 = new JLabel();
      jlabel2.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel2.setText("Input (CR terminated)");
      jpanel1.add(jlabel2,cc.xy(4,6));

      JLabel jlabel3 = new JLabel();
      jlabel3.setBackground(new Color(204,204,204));
      jlabel3.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel3.setOpaque(true);
      jlabel3.setText("State ");
      jlabel3.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel3,new CellConstraints(4,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _input.setName("input");
      jpanel1.add(_input,cc.xywh(6,6,15,1));

      _START_BUTTON.setActionCommand("Start");
      _START_BUTTON.setName("START_BUTTON");
      _START_BUTTON.setToolTipText("Start");
      jpanel1.add(_START_BUTTON,cc.xy(4,18));

      _STOP_BUTTON.setActionCommand("Stop");
      _STOP_BUTTON.setName("STOP_BUTTON");
      _STOP_BUTTON.setToolTipText("Stop");
      jpanel1.add(_STOP_BUTTON,cc.xy(6,18));

      _RESTART_BUTTON.setActionCommand("Restart");
      _RESTART_BUTTON.setName("RESTART_BUTTON");
      _RESTART_BUTTON.setToolTipText("Restart");
      jpanel1.add(_RESTART_BUTTON,cc.xy(8,18));

      _EXIT_WRAPPER_BUTTON.setActionCommand("Exit");
      _EXIT_WRAPPER_BUTTON.setName("EXIT_WRAPPER_BUTTON");
      _EXIT_WRAPPER_BUTTON.setToolTipText("Stop Wrapper");
      jpanel1.add(_EXIT_WRAPPER_BUTTON,new CellConstraints(20,18,1,1,CellConstraints.RIGHT,CellConstraints.DEFAULT));

      _THREAD_DUMP_BUTTON.setActionCommand("Thread Dump");
      _THREAD_DUMP_BUTTON.setName("THREAD_DUMP_BUTTON");
      _THREAD_DUMP_BUTTON.setToolTipText("Thread Dump");
      jpanel1.add(_THREAD_DUMP_BUTTON,new CellConstraints(10,18,1,1,CellConstraints.LEFT,CellConstraints.DEFAULT));

      _appStopTime.setName("appStopTime");
      _appStopTime.setText("-");
      jpanel1.add(_appStopTime,cc.xy(10,11));

      JLabel jlabel4 = new JLabel();
      jlabel4.setBackground(new Color(204,204,204));
      jlabel4.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel4.setOpaque(true);
      jlabel4.setText("Started");
      jpanel1.add(jlabel4,new CellConstraints(8,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _state.setBackground(new Color(255,255,255));
      _state.setName("state");
      _state.setText("IDLE");
      jpanel1.add(_state,cc.xy(4,11));

      JLabel jlabel5 = new JLabel();
      jlabel5.setBackground(new Color(204,204,204));
      jlabel5.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel5.setOpaque(true);
      jlabel5.setText("Stopped");
      jpanel1.add(jlabel5,new CellConstraints(10,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _wStartTime.setName("wStartTime");
      _wStartTime.setText("-");
      jpanel1.add(_wStartTime,cc.xy(18,11));

      _trigger.setName("trigger");
      _trigger.setText("-");
      jpanel1.add(_trigger,cc.xy(20,11));

      JLabel jlabel6 = new JLabel();
      jlabel6.setBackground(new Color(204,204,204));
      jlabel6.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel6.setOpaque(true);
      jlabel6.setText("PID");
      jpanel1.add(jlabel6,new CellConstraints(16,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      JLabel jlabel7 = new JLabel();
      jlabel7.setBackground(new Color(204,204,204));
      jlabel7.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel7.setOpaque(true);
      jlabel7.setText("Started");
      jpanel1.add(jlabel7,new CellConstraints(18,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      JLabel jlabel8 = new JLabel();
      jlabel8.setBackground(new Color(204,204,204));
      jlabel8.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel8.setOpaque(true);
      jlabel8.setText("Trigger");
      jpanel1.add(jlabel8,new CellConstraints(20,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _STOP_TIMER_BUTTON.setActionCommand("Stop Timer / Condition");
      _STOP_TIMER_BUTTON.setName("STOP_TIMER_BUTTON");
      _STOP_TIMER_BUTTON.setToolTipText("Stop Timer / Condition");
      jpanel1.add(_STOP_TIMER_BUTTON,new CellConstraints(18,18,1,1,CellConstraints.RIGHT,CellConstraints.DEFAULT));

      _appPid.setName("appPid");
      _appPid.setText("-");
      jpanel1.add(_appPid,cc.xy(6,11));

      _appStartTime.setName("appStartTime");
      _appStartTime.setText("-");
      jpanel1.add(_appStartTime,cc.xy(8,11));

      _wPid.setName("wPid");
      _wPid.setText("-");
      jpanel1.add(_wPid,cc.xy(16,11));

      _titledborderlabel1.setText("Application");
      jpanel1.add(_titledborderlabel1,cc.xywh(3,8,10,1));

      jpanel1.add(_titledborderside1,cc.xywh(2,8,1,12));

      _titledborderside2.setOrientation(TitledBorderSide.RIGHT);
      jpanel1.add(_titledborderside2,cc.xywh(13,8,1,12));

      jpanel1.add(_titledborderbottom1,cc.xywh(3,19,10,1));

      _titledborderlabel2.setText("Wrapper");
      jpanel1.add(_titledborderlabel2,cc.xywh(15,8,6,1));

      jpanel1.add(_titledborderside3,cc.xywh(14,8,1,12));

      _titledborderside4.setOrientation(TitledBorderSide.RIGHT);
      jpanel1.add(_titledborderside4,cc.xywh(21,8,1,12));

      jpanel1.add(_titledborderbottom2,cc.xywh(15,19,6,1));

      _jbutton1.setActionCommand("Close Console");
      _jbutton1.setText("Close Console");
      _jbutton1.setToolTipText("Close Console The Console Window");
      jpanel1.add(_jbutton1,cc.xy(20,21));

      _timer.setName("timer");
      _timer.setText("-");
      jpanel1.add(_timer,cc.xy(16,15));

      _condition.setName("condition");
      _condition.setText("-");
      jpanel1.add(_condition,cc.xy(18,15));

      _wrapperType.setName("wrapperType");
      _wrapperType.setText("-");
      jpanel1.add(_wrapperType,cc.xy(20,15));

      JLabel jlabel9 = new JLabel();
      jlabel9.setBackground(new Color(204,204,204));
      jlabel9.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel9.setOpaque(true);
      jlabel9.setText("PID");
      jlabel9.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel9,new CellConstraints(6,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      JLabel jlabel10 = new JLabel();
      jlabel10.setBackground(new Color(204,204,204));
      jlabel10.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel10.setOpaque(true);
      jlabel10.setText("Timer");
      jlabel10.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel10,new CellConstraints(16,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      JLabel jlabel11 = new JLabel();
      jlabel11.setBackground(new Color(204,204,204));
      jlabel11.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel11.setOpaque(true);
      jlabel11.setText("Condition");
      jlabel11.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel11,new CellConstraints(18,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      JLabel jlabel12 = new JLabel();
      jlabel12.setBackground(new Color(204,204,204));
      jlabel12.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel12.setOpaque(true);
      jlabel12.setText("Type");
      jlabel12.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel12,new CellConstraints(20,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      JLabel jlabel13 = new JLabel();
      jlabel13.setBackground(new Color(204,204,204));
      jlabel13.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel13.setOpaque(true);
      jlabel13.setText("CPU");
      jlabel13.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel13,new CellConstraints(4,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _cpu.setName("cpu");
      _cpu.setText("-");
      jpanel1.add(_cpu,cc.xy(4,15));

      JLabel jlabel14 = new JLabel();
      jlabel14.setBackground(new Color(204,204,204));
      jlabel14.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel14.setOpaque(true);
      jlabel14.setText("Memory");
      jlabel14.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel14,new CellConstraints(6,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _memory.setName("memory");
      _memory.setText("-");
      jpanel1.add(_memory,cc.xy(6,15));

      JLabel jlabel15 = new JLabel();
      jlabel15.setBackground(new Color(204,204,204));
      jlabel15.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel15.setOpaque(true);
      jlabel15.setText("Handles");
      jlabel15.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel15,new CellConstraints(8,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _handles.setName("handles");
      _handles.setText("-");
      jpanel1.add(_handles,cc.xy(8,15));

      JLabel jlabel16 = new JLabel();
      jlabel16.setBackground(new Color(204,204,204));
      jlabel16.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel16.setOpaque(true);
      jlabel16.setText("Threads");
      jlabel16.setHorizontalAlignment(JLabel.LEFT);
      jpanel1.add(jlabel16,new CellConstraints(10,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _threads.setName("threads");
      _threads.setText("-");
      jpanel1.add(_threads,cc.xy(10,15));

      _count.setName("count");
      _count.setText("-");
      jpanel1.add(_count,cc.xy(12,15));

      JLabel jlabel17 = new JLabel();
      jlabel17.setBackground(new Color(204,204,204));
      jlabel17.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel17.setOpaque(true);
      jlabel17.setText("Restarts");
      jpanel1.add(jlabel17,new CellConstraints(12,13,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      JLabel jlabel18 = new JLabel();
      jlabel18.setBackground(new Color(204,204,204));
      jlabel18.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel18.setOpaque(true);
      jlabel18.setText("Exit Code");
      jpanel1.add(jlabel18,new CellConstraints(12,9,1,1,CellConstraints.FILL,CellConstraints.DEFAULT));

      _exitCode.setName("exitCode");
      _exitCode.setText("-");
      jpanel1.add(_exitCode,cc.xy(12,11));

      _THREAD_DUMP_WRAPPER_BUTTON.setActionCommand("Exit");
      _THREAD_DUMP_WRAPPER_BUTTON.setName("THREAD_DUMP_WRAPPER_BUTTON");
      _THREAD_DUMP_WRAPPER_BUTTON.setToolTipText("Thread Dump Wrapper");
      jpanel1.add(_THREAD_DUMP_WRAPPER_BUTTON,new CellConstraints(16,18,1,1,CellConstraints.RIGHT,CellConstraints.DEFAULT));

      _EXIT_TRAY_ICON_BUTTON.setActionCommand("Exit");
      _EXIT_TRAY_ICON_BUTTON.setName("EXIT_TRAY_ICON_BUTTON");
      _EXIT_TRAY_ICON_BUTTON.setToolTipText("Exit Tray Icon");
      jpanel1.add(_EXIT_TRAY_ICON_BUTTON,new CellConstraints(18,21,1,1,CellConstraints.RIGHT,CellConstraints.DEFAULT));

      _START_OUTPUT_BUTTON.setEnabled(false);
      _START_OUTPUT_BUTTON.setName("START_OUTPUT_BUTTON");
      _START_OUTPUT_BUTTON.setToolTipText("Start Console Output");
      jpanel1.add(_START_OUTPUT_BUTTON,cc.xy(6,2));

      _PAUSE_OUTPUT_BUTTON.setName("PAUSE_OUTPUT_BUTTON");
      _PAUSE_OUTPUT_BUTTON.setToolTipText("Pause Console Output");
      jpanel1.add(_PAUSE_OUTPUT_BUTTON,cc.xy(8,2));

      JLabel jlabel19 = new JLabel();
      jlabel19.setFont(new Font("Tahoma",Font.BOLD,11));
      jlabel19.setText("Filter");
      jpanel1.add(jlabel19,cc.xy(16,2));

      __OUTPUT_FILTER.setName("_OUTPUT_FILTER");
      jpanel1.add(__OUTPUT_FILTER,cc.xywh(18,2,3,1));

      _CLEAR_OUTPUT_BUTTON.setName("CLEAR_OUTPUT_BUTTON");
      _CLEAR_OUTPUT_BUTTON.setToolTipText("Clear Output");
      jpanel1.add(_CLEAR_OUTPUT_BUTTON,cc.xy(10,2));

      _GC_BUTTON.setActionCommand("Thread Dump");
      _GC_BUTTON.setName("GC_BUTTON");
      _GC_BUTTON.setToolTipText("Thread Dump");
      jpanel1.add(_GC_BUTTON,cc.xy(12,18));

      _DUMP_HEAP_BUTTON.setActionCommand("Thread Dump");
      _DUMP_HEAP_BUTTON.setName("DUMP_HEAP_BUTTON");
      _DUMP_HEAP_BUTTON.setToolTipText("Thread Dump");
      jpanel1.add(_DUMP_HEAP_BUTTON,cc.xy(10,17));

      addFillComponents(jpanel1,new int[]{ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21 },new int[]{ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22 });
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
