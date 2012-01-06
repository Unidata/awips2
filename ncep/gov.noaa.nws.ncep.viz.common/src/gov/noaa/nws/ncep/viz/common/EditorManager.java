package gov.noaa.nws.ncep.viz.common;

public class EditorManager {
	/*
	 * editorNumList element definition:
	 *  index: index+1 act as editor number of index. For example, index 3 is used to store info of editor number 4 (=3+1).
	 *  first column int value: used to store the availability of the number. 0 is available, 1 is used.
	 *  2nd column int value: used to store the number of instance of that particular editor.
	 */
	static int listLength = 100;
	protected static int[][] editorNumList= new int[listLength][2];
	
    public static void initEditorNumList(){
    	for (int i =0; i< listLength; i++){
    		editorNumList[i][0] = 0;
    		editorNumList[i][1] = 0;
    	}
    	//System.out.println("EditorIdNumberManager inited!!!!!");
    }
    public static int getEditorNumber(){
    	for (int i =0; i< listLength; i++){
    		if(editorNumList[i][0] == 0){
    			editorNumList[i][0] = 1;
    			//System.out.println("getEditorNumber "+ (i+1));
    			return i+1;
    		}
    	}
    	return 0;
    }
    public static void returnEditorNumber(int editorNum){
    	//System.out.println("returnEditorNumber "+ editorNum);
    	if(editorNum <= listLength && editorNum >0){
    		editorNumList[editorNum-1][0] = 0;
    		editorNumList[editorNum-1][1] = 0;
    	}
    }

    /*
     * Return value:
     * = -1: the input editor num is not good or is not allocated yet.
     * >0:  the number of editor instances is registered, including the just registered one
     */
    public static int registerEditorNumber(int editorNum){
    	//System.out.print("registerEditorNumber "+ editorNum);
    	if(editorNum <= listLength && editorNum >0 && editorNumList[editorNum-1][0]== 1){
    		editorNumList[editorNum-1][1]++;
    		//System.out.println(" current instanec # "+ editorNumList[editorNum-1][1]);
    		return editorNumList[editorNum-1][1];
    	}
    	else
    		return -1;
    }
    
    /*
     * 
     * Return value:
     * =-1: the input editor num is not good or is not allocated yet.
     * =0: No more editor instance is using this number. The number will be returned and become available as well .
     * >0:  the number of editor instances is registered, including the just unregistered one
     */
    public static int unregisterEditorNumber(int editorNum){
    	//System.out.print("UNregisterEditorNumber "+ editorNum);
    	if(editorNum <= listLength && editorNum >0 && editorNumList[editorNum-1][0]== 1 && editorNumList[editorNum-1][1]>0){
    		editorNumList[editorNum-1][1]--;
    		if(editorNumList[editorNum-1][1] == 0){
    			editorNumList[editorNum-1][0] = 0; // set the number to be available
    		}
    		//System.out.println(". After unregistered, current instanec # "+ editorNumList[editorNum-1][1]);
    		return editorNumList[editorNum-1][1];
    	}
    	else
    		return -1;
    }

    /*
     * return the number of instances of the editor which is using the input editor number
     * Input: editorNum 
     * Return value: the number of instance of this editor
     *  
     *
     */
    public static int getNumOfEditorInstance(int editorNum) {
    	if(editorNum <= listLength && editorNum >0 ){
    		return editorNumList[editorNum-1][1];
    	}
    	else
    		return 0;
    }
}
