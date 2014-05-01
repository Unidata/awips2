package ohd.hseb.raxbase.util;

import ohd.hseb.raxbase.RaxBaseDataMgr;

/**
* Pass in a dialog to the constructor, and this is ready to be inserted into a Thread.  When
* run, it just makes the dialog visible.  This allows for a non-blocking modal dialog to be
* created that can display progress while allowing the underlying process to continue execution.
* Normally, when a modal dialog is made visible, it blocks the working processing until closed.
* @author hank
*/
public class ModalDialogRunner extends Thread
{
   
   private RaxBaseDataMgr _dataMgr = null;
   
   public ModalDialogRunner( RaxBaseDataMgr dataMgr )
   {
       _dataMgr = dataMgr;
   }
   
   public void run()
   {
       _dataMgr.setRaxIngestFilterRowDataList( _dataMgr.createIngestFilterRowDataList() );
       _dataMgr.finishProcess();
   }
} 