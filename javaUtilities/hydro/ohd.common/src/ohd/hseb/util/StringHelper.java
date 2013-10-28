package ohd.hseb.util;

public class StringHelper
{
    public static int areArraysEqual(String[] array1, String[] array2)
    {
        int result = 0;

        if(array1 == null && array2 != null)
            result = -1;
        else if(array2 == null && array1 != null)
            result = -1;
        else if(array1 == null && array2 == null)
            result = 0;
        else if(array1.length != array2.length)
        {
            result = -1 ;
        }
        else
        {
            for(int i=0 ; i < array1.length; i++)
            {
                if(array1[i].compareTo(array2[i]) != 0)
                {
                    result = -1;
                    break;
                }
            }
        }
        return result;
    }


    public static String capitalizeFirstLetterOfWords(String text)
    {
        String result = null;
        if(text != null)
        {
            String tempText = text.toLowerCase();
            String splitWords[] = tempText.split("\\s+");

            if(splitWords != null)
            {
                String newConvertedWords[] = new String[splitWords.length];

                for(int i=0; i < splitWords.length; i++)
                {
                    boolean newWordFormed = false;
                    String newWord = null;
                    String orgWord = splitWords[i];
                    if(orgWord.length() != 0)
                    {
                        char begChar = orgWord.charAt(0);
                        if(Character.isLetter(begChar))
                        {
                            if(Character.isLowerCase(begChar))
                            {
                                char newBegChar = Character.toUpperCase(begChar);
                                newWord = newBegChar + orgWord.substring(1, orgWord.length());
                                newWordFormed = true;
                            }
                        }
                    }
                    if (newWordFormed)
                    {
                        newConvertedWords[i] = newWord;
                    }
                    else
                    {
                        newConvertedWords[i] = orgWord;
                    }
                }

                result = "";

                for(int i=0; i < newConvertedWords.length; i++)
                {
                    if(i == 0 )
                    {
                        result =  newConvertedWords[i];
                    }
                    else
                    {
                        result = result + " " + newConvertedWords[i];
                    }
                }
            }
        }

        return result;
    }

}
