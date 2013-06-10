package ohd.hseb.util.gui.jtable.example;

/**
 * Creates example data for use in the jtable example application
 * 
 * @author rajaramv
 *
 */
public class ExampleData
{
    private int _numOfRows = 10;
    private static int _count = 0;

    protected int[] getExampleIdValues()
    {

        int[] result = new int[_numOfRows];

        for(int i=0; i < _numOfRows; i++)
        {
            result [i] =  i + _count ++; 
        }

        return result;
    }

    protected short[] getExampleAgeValues()
    {

        short[] result = new short[_numOfRows];

        for(int i=0; i < _numOfRows; i++)
        {
            result [i] =(short) i; 
        }

        return result;
    }

    protected long[] getExampleDateOfBirthValues()
    {

        long[] result = new long[_numOfRows];

        for(int i=0; i < _numOfRows; i++)
        {
            result [i] = (long) i*10000; 
        }

        return result;
    }
    
    protected long[] getExampleSalaryValues()
    {

        long[] result = new long[_numOfRows];

        for(int i=0; i < _numOfRows; i++)
        {
            result [i] = (long) i*10000; 
        }

        return result;
    }

    protected float[] getExampleHeightValues()
    {

        float[] result = new float[_numOfRows]; 

        for(int i=0; i < _numOfRows; i++)
        {
            result [i] = (float) i; 
        }

        return result;
    }


    protected double[] getExampleWeightValues()
    {

        double[] result = new double[_numOfRows];

        for(int i=0; i < _numOfRows; i++)
        {
            result [i] = (double) i; 
        }

        return result;
    }


    protected boolean[] getExampleMaritalStatusValues()
    {

        boolean[] result = new boolean[_numOfRows];

        for(int i=0; i < _numOfRows; i++)
        {
            if((i / 2) == 0)
                result [i] = true;
            else
                result [i] = false;
        }

        return result;
    }

    protected String[] getExampleNameValues()
    {

        String[] result = new String[_numOfRows];

        for(int i=0; i < _numOfRows; i++)
        {
            if((i / 2) == 0)
                result [i] = "Hello" + i;
            else
                result [i] = "Hello" + 1 + (i +1);
        }

        return result;
    }

}
