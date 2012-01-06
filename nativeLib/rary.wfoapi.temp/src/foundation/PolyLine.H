#ifndef PolyLine_H
#define PolyLine_H

#include "SeqOf.H"
#include "CartCoord2D.H"

template<class T>
class PolyLine : public SeqOf<CartCoord2D<T> >
    {
    public:
        PolyLine(void);
// The HP compiler barfs on one of these.
// I'll figure it out when I have the time.
#if 0
        PolyLine(const T &t);
        PolyLine(const T *vector, int length);
        PolyLine(const PolyLine &line);
#endif
        PolyLine(const SeqOf<CartCoord2D<T> > &line);
        virtual ~PolyLine(void);

#if 0
        PolyLine& operator=(const PolyLine &rhs) 
            { SeqOf<CartCoord2D<T> >::operator=(rhs); return *this; }
#endif

        virtual int findClosest(const CartCoord2D<T> &point) const;
    };

template <class T>
PolyLine<T>::PolyLine(void) : SeqOf<CartCoord2D<T> >()
    {
    }

#if 0
template <class T>
PolyLine<T>::PolyLine(const T &t) : SeqOf<CartCoord2D<T> >(t)
    {
    }

template <class T>
PolyLine<T>::PolyLine(const T *vector, int length) : 
  SeqOf<CartCoord2D<T> >(vector, length)
    {
    }


template <class T>
PolyLine<T>::PolyLine(const PolyLine<T> &line) :
  SeqOf<CartCoord2D<T> >(line)
    {
    }
#endif

template <class T>
PolyLine<T>::PolyLine(const SeqOf<CartCoord2D<T> > &line) :
  SeqOf<CartCoord2D<T> >(line)
    {
    }

template <class T>
PolyLine<T>::~PolyLine(void)
    {
    }

template <class T>
int PolyLine<T>::findClosest(const CartCoord2D<T> &point) const
    {
    if (this->length() == 0)
        return -1;

    T minlength = (*this)[0].distance(point);
    int index = 0;
    for (int i = 1 ; i < this->length() ; i++)
        {
        T l = (*this)[i].distance(point);
        if (l < minlength)
            {
            index = i;
            minlength = l;
            }
        }

    return index;
    }

#endif
