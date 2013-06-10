package test;


public class HelloWorld2 extends HelloWorld
{

	public static void main(String[] args)
	{
		try
		{
			Thread.sleep(30000);
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.exit(99);
	}

}
