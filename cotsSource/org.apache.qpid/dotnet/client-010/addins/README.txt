This project contains three sub-projects: 
- The RTD excell Addin 
- A sample client sending messages to queue1
- A ample message processor 

RDT AddIn
Excel provides a function called RTD (real-time data) that lets you specify a COM server via its ProgId here "Qpid" so that you can push qpid messages into Excel. 
For using the Qpid RTD follows those steps: 

1) Copy the configuration Excel.exe.config into C:\Program Files\Microsoft Office\Office12 
2) Edit Excel.exe.xml and set the targeted Qpid broker host, port number 
3) Select the cell or cell range to contain the information 
4) enter the following formula =rtd("Qpid",,"myQueue") Where MyQueue is the queue from which you wish to receive messages from 

Note: The Qpid  RTD is a COM-AddIn that must be registered with Excel. This is done automatically when compiling the Addin with visual studio. 

The default behavior of the RDT AddIn is to display the message payload. This could be altered by specifying your own message processor. 
A Message processor is a class that implements the API ExcelAddIn.MessageProcessor. For example, the provided processor in client-010\addins\ExcelAddInMessageProcessor displays the message body and the the header price when specified. 

To use you own message processor follows those steps: 
1) Write your own message processor that extends ExcelAddIn.MessageProcessor
2) Edit Excel.exe.config and uncomment the entries: 
 <add key="ProcessorAssembly" value="<path>\qpid\dotnet\client-010\addins\ExcelAddInMessageProcessor\bin\Debug\ExcelAddInMessageProcessor.dll"/>
 <add key="ProcessorClass" value="ExcelAddInMessageProcessor.Processor"/> 
- ProcessorAssembly is the path on the Assembly that contains your processor class 
- ProcessorClass is your processor class name 
3) run excel and define a rtd function 

Note: the provided ExcelAddInProducer can be used for testing the provided message processor. As messages are sent to queue1 the following rtd fucntion should be used =rtd("Qpiud",,"queue1")