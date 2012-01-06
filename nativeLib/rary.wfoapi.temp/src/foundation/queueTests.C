// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// queueTests.C
// A program that tests the functionality of the Queue and QueuePtr classes.
//
// Author: The Irish Egoless Programmer.
// ---------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const queueTests_C_Id =
  "$Id: .queueTests.C__temp27950,v 1.2 2003/05/06 23:11:59 fluke Exp $";
#endif


#include "Queue.H"
#include "QueuePtr.H"
#include <iostream>
#include <limits.h>
#include <stdlib.h>

void testQueue (void)
    {
    //describe what we are testing.
    std::cout << std::endl << "Now testing a queue of data!"
         << std::endl << std::endl;

    Queue<int> q;

    //First TEST, fill the queue with the first ten odd integers, 
    //then remove and print each value until the queue is empty.
    std::cout << "First TEST, fill the queue with the first ten"
         << " odd integers," << std::endl;
    std::cout << "then remove and print each value until the queue"
         << " is empty." << std::endl;

    int i;
    for (i = 1; i < 20; i+=2)
    q.insert (i);
    while (!q.isEmpty ())
    std::cout << q.remove () << " ";
    std::cout << std::endl << std::endl;

    //Second TEST, fill the queue with the first ten even integers,
    //then peek and print, then remove and print until the queue is
    //empty.
    std::cout << "Second TEST, fill the queue with "
         << "the first ten even integers," << std::endl;
    std::cout << "then then peek and print, then remove and "
         << "print until the queue is empty." << std::endl;
    for (i = 0; i < 20; i+=2)
    q.insert (i);
    while (!q.isEmpty ())
    {
        std::cout << q.peek () << " ";
        std::cout << q.remove () << " ";
    }
    std::cout << std::endl << std::endl;

    //Third TEST, try to remove from an empty queue.
    std::cout << "Third TEST, try to remove from an empty queue."
         << std::endl;
    q.remove ();
    std::cout << std::endl;
    
    //Fourth TEST, try to peek from an empty queue.
    std::cout << "Fourth TEST, try to peek from an empty queue."
         << std::endl;
    q.peek ();
    std::cout << std::endl;

    //Fifth TEST, fill up the queue and try to insert into it.  Note:
    //This test will only succeed if the queue is indexed by a character
    //rather than a long.
    std::cout << "Fifth TEST, fill up the queue "
         << " and try to insert into it." << std::endl;
    for (i = 0; i < UCHAR_MAX; i++)
        q.insert (i);
    q.insert (i);
    std::cout << std::endl;
    }


void testPtrQueue (void)
    {
    //describe what we are testing.
    std::cout << std::endl << "Now testing a queue of pointers to data!"
         << std::endl << std::endl;

    QueuePtr <int *> q;

    //First TEST, fill the queue with the first ten odd integers, 
    //then remove and print each value until the queue is empty.
    std::cout << "First TEST, fill the queue with the first ten"
         << " odd integers," << std::endl;
    std::cout << "then remove and print each value until the queue"
         << " is empty." << std::endl;

    int odd[10];
    int i;
    for (i = 0; i < 10; i++)
        {
        odd[i] = i * 2 + 1;
        q.insert (&odd[i]);
        }
    while (!q.isEmpty ())
        {
        int *val = q.remove ();
        std::cout << *val << " ";
        }
    std::cout << std::endl << std::endl;

    //Second TEST, fill the queue with the first ten even integers,
    //then peek and print, then remove and print until the queue is
    //empty.
    std::cout << "Second TEST, fill the queue with "
         << "the first ten even integers," << std::endl;
    std::cout << "then then peek and print, then remove and "
         << "print until the queue is empty." << std::endl;
    for (i = 0; i < 10; i++)
        {
        int even[10];
        even[i] = i * 2;
        q.insert (&even[i]);
        }
    while (!q.isEmpty ())
        {
        int *peekVal = q.peek ();
        int *val = q.remove ();
        std::cout << *peekVal << " "  << *val << " ";
        }
    std::cout << std::endl << std::endl;

    //Third TEST, try to remove from an empty queue.
    std::cout << "Third TEST, try to remove from an empty queue."
         << std::endl;
    q.remove ();
    std::cout << std::endl;
    
    //Fourth TEST, try to peek from an empty queue.
    std::cout << "Fourth TEST, try to peek from an empty queue."
         << std::endl;
    q.peek ();
    std::cout << std::endl;

    //Fifth TEST, fill up the queue and try to insert into it.  Note:
    //This test will only succeed if the queue is indexed by a character
    //rather than a long.
    std::cout << "Fifth TEST, fill up the queue "
         << " and try to insert into it." << std::endl;
    for (i = 0; i < UCHAR_MAX; i++)
        q.insert (&i);
    q.insert (&i);
    std::cout << std::endl;
    }

int main (void)
    {
    testQueue ();
    testPtrQueue ();
    return 0;
    }

