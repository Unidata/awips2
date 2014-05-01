package ohd.hseb.raxdb_sync;

import ohd.hseb.db.DbRecord;
import ohd.hseb.raxbase.model.RaxRating;
import ohd.hseb.raxbase.model.RaxRatingShift;

public class RatingCurveHolder extends DbRecord
{

    private RaxRating _ratingCurve = null;
    private RaxRatingShift _ratingShift = null;
    
    public RatingCurveHolder()
    {
    }

    public RatingCurveHolder(RaxRating ratingCurve, RaxRatingShift ratingShift)
    {
        setRatingCurve(ratingCurve);
        setRatingShift(ratingShift);
    }
 
    private void setRatingCurve(RaxRating ratingCurve)
    {
        _ratingCurve = ratingCurve;
    }
    
    public RaxRating getRatingCurve()
    {
        return _ratingCurve;
    }

    private void setRatingShift(RaxRatingShift ratingShift)
    {
        _ratingShift = ratingShift;
    }
    
    public RaxRatingShift getRatingShift()
    {
        return _ratingShift;
    }

    @Override
    public String toString()
    {
        // TODO Auto-generated method stub
        return null;
    }

}
