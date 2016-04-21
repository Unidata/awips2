// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// SeqOf.H
// General-purpose Tree class template.
//
// Author: romberg
// ---------------------------------------------------------------------------
#ifndef Tree_H
#define Tree_H

#include "SeqOfPtr.H"
#include <iostream>

class TreeWalker
    {
    };

template <class T>
class Tree
    {
    public:
        Tree(const T &data);
        Tree();
        Tree(const Tree<T> &t);
        virtual ~Tree();
        
        Tree &operator=(const Tree &t);

        const T &data() const { return _data; }
        T &data() { return _data; }
        operator const T &() const { return _data; }
        operator T &() { return _data; }

        std::ostream &printOn(std::ostream &os) const;

        typedef void (TreeWalker::*WalkCBconst)(const T &t, void *data);
        typedef void (TreeWalker::*WalkCB)(T &t, void *data);
        void walkDepth(TreeWalker *obj, WalkCBconst fn, void *data = 0) const;
        void walkDepth(TreeWalker *obj, WalkCB fn, void *data = 0);
        void walkBreadth(TreeWalker *obj, WalkCBconst fn, 
          void *data = 0) const;
        void walkBreadth(TreeWalker *obj, WalkCB fn, 
          void *data = 0);

        bool equals(const Tree<T> &t, bool orderImportant = true) const;
        bool operator==(const Tree<T> &t) const { return equals(t); }
        bool operator!=(const Tree<T> &t) const { return !equals(t); }

        bool contains(const T &t) const;

        // SeqOf<T> "type" methods.
        int length() const { return _children.length(); }
        void append(const T &t) { _children.append(newNode(t)); }
        void append(const SeqOf<T> &list);
        void prepend(const T &t) { _children.prepend(newNode(t)); }
        void prepend(const SeqOf<T> &list);
        void remove(int lo, int hi);
        Tree<T> &operator[](int index) { return *_children[index]; }
        const Tree<T> &operator[](int index) const 
            { return *_children[index]; }
        int find(const T &t, int startPos = 0) const;

    protected:
        // Simulate a virtual ctors for derrived classes.
        // This function should return a pointer to a new instance
        // of the derrived Tree
        virtual Tree *newNode(const T &t) const { return new Tree<T>(t); }

    private:
        SeqOfPtr<Tree<T> *> _children;
        T _data;

        void copyTree(const Tree<T> &t);
        void deleteTree();
    };

// -- global -----------------------------------------------------------------
// template <class T>
// ::operator<<()
//
// Ostream insertion operator for a Tree<T>.
//
// -- implementation ---------------------------------------------------------
// Use Tree<T>::printOn().
// ---------------------------------------------------------------------------
template <class T>
inline std::ostream &operator<<(std::ostream &os, const Tree<T> &t)
    {
    return t.printOn(os);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::Tree()
//
// Constructor for a Tree<T>.  'data' is the data to be stored in this node.
//
// -- implementation ---------------------------------------------------------
// Save the data.
// ---------------------------------------------------------------------------
template <class T>
Tree<T>::Tree(const T &data) : _data(data)
    {
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::Tree()
//
// Default constructor for a Tree<T>.
//
// -- implementation ---------------------------------------------------------
// Init _data using the default constructor for type T.
// ---------------------------------------------------------------------------
template <class T>
Tree<T>::Tree() : _data(T())
    {
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::Tree()
//
// Copy constructor for Tree<T>.  Takes a const reference to another Tree.
//
// -- implementation ---------------------------------------------------------
// Copy the other trees data into _data.  Then copy the rest via a call to
// copyTree().
// ---------------------------------------------------------------------------
template <class T>
Tree<T>::Tree(const Tree<T> &t) : _data(t._data)
    {
    copyTree(t);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::~Tree()
//
// Destructor for Tree<T>.
//
// -- implementation ---------------------------------------------------------
// Delete all children via a call to deleteTree().
// ---------------------------------------------------------------------------
template <class T>
Tree<T>::~Tree()
    {
    deleteTree();
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::operator=()
//
// Assignment operator for Tree<T>.
//
// -- implementation ---------------------------------------------------------
// Remove all children via a call to deleteTree().  Then copy the other
// Tree with copyTree().  Finally, copy this nodes data into _data.
// ---------------------------------------------------------------------------
template <class T>
Tree<T> &Tree<T>::operator=(const Tree<T> &t)
    {
    if (this == &t )
        return *this;
    deleteTree();
    copyTree(t);
    _data = t._data;
    return *this;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::equals()
//
// Compare this Tree with the supplied tree.  The flag orderImportant, is
// used to tell the method if the order of the children is important or
// if order should be ignored (just set type comparisons).
//
// -- implementation ---------------------------------------------------------
// The two trees must have the same number of children.
// - If order is important, then equals() must be true for each index
//   in this tree and the supplied tree.
// - If order is not important, then each child of this tree must exist
//   in the supplied tree and equals(, false) must hold true for each subtree.
// ---------------------------------------------------------------------------
template <class T>
bool Tree<T>::equals(const Tree<T> &t, bool orderImportant) const
    {
    if ((t.data() != data()) || (t.length() != length()))
        return false;

    if (orderImportant)
        {
        for (int i = 0 ; i < length() ; i++)
            if ((*this)[i] != t[i])
                return false;
        }
    else
        {
        for (int i = 0 ; i < length() ; i++)
            {
            int index = t.find(*_children[i]);
            if (index == -1)
                return false;
            if (!t._children[index]->equals(*_children[i], false))
                return false;
            }
        }

    return true;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::printOn()
//
// Prints this Tree on an ostream.
//
// -- implementation ---------------------------------------------------------
// Print _data and then call printOn() for each child.
// ---------------------------------------------------------------------------
template <class T>
std::ostream &Tree<T>::printOn(std::ostream &os) const
    {
    os << "data: " << _data << "\n" << "# branches: " << _children.length();
    if (_children.length())
        os << "\n";

    for (int i = 0 ; i < _children.length() ; i++)
        {
        _children[i]->printOn(os);
        if (i != _children.length() - 1)
            os << "\n";
        }
    
    return os;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::walkDepth()
//
// Walks this Tree (depth first) calling the supplied member function for
// each node visited.  User supplied data is passed as 'data'.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::walkDepth(TreeWalker *obj, WalkCBconst fn, void *data) const
    {
    for (int i = 0 ; i < _children.length() ; i++)
        _children[i]->walkDepth(obj, fn, data);
    
    (obj->*fn)(_data, data);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::walkDepth()
//
// Walks this Tree (depth first) calling the supplied member function for
// each node visited.  User supplied data is passed as 'data'.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::walkDepth(TreeWalker *obj, WalkCB fn, void *data)
    {
    for (int i = 0 ; i < _children.length() ; i++)
        _children[i]->walkDepth(obj, fn, data);
    
    (obj->*fn)(_data, data);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::walkBreadth()
//
// Walks this Tree (breadth first) calling the supplied member function for
// each node visited.  User supplied data is passed as 'data'.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::walkBreadth(TreeWalker *obj, WalkCBconst fn, void *data) 
  const
    {
    (obj->*fn)(_data, data);

    for (int i = 0 ; i < _children.length() ; i++)
        _children[i]->walkBreadth(obj, fn, data);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::walkBreadth()
//
// Walks this Tree (breadth first) calling the supplied member function for
// each node visited.  User supplied data is passed as 'data'.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::walkBreadth(TreeWalker *obj, WalkCB fn, void *data)
    {
    (obj->*fn)(_data, data);

    for (int i = 0 ; i < _children.length() ; i++)
        _children[i]->walkBreadth(obj, fn, data);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::append()
//
// Append new branches with data specified by list.
//
// -- implementation ---------------------------------------------------------
// Use append(const T &).
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::append(const SeqOf<T> &list)
    {
    for (int i = 0 ; i < list.length() ; i++)
        append(list[i]);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::prepend()
//
// Prepend new branches with data specified by list.
//
// -- implementation ---------------------------------------------------------
// Use prepend(const T &).
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::prepend(const SeqOf<T> &list)
    {
    for (int i = 0 ; i < list.length() ; i++)
        prepend(list[i]);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::remove()
//
// Remove the branches specified by lo and hi.
//
// -- implementation ---------------------------------------------------------
// Call deleteTree() on each branch from lo to hi.  Then call 
// _children.remove(lo, hi).
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::remove(int lo, int hi)
    {
    for (int i = lo ; i <= hi ; i++)
        delete _children[i];

    _children.remove(lo, hi);
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::find()
//
// SeqOf like find() method for a Tree.  Just looks for the supplied value
// in the immediate list of children (not recursive).
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template <class T>
int Tree<T>::find(const T &t, int startPos) const
    {
    // If startPos is out of range then abort.
    if ((startPos < 0) || (startPos >= length()))
        return -1;

    // Try and find a match
    for (int i = startPos ; i < length() ; i++)
        if (_children[i]->data() == t)
            return i;

    // No match found
    return -1;
    }

// -- public -----------------------------------------------------------------
// template <class T>
// Tree<T>::contains()
//
// Returns true if the supplied object can be found in this tree and false
// if not.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
template <class T>
bool Tree<T>::contains(const T &t) const
    {
    if (data() == t)
        return true;

    for (int i = 0 ; i < length() ; i++)
        if ((*this)[i].contains(t))
            return true;

    return false;
    }

// -- private ---------------------------------------------------------------
// template <class T>
// Tree<T>::copyTree()
//
// Copies into this Tree the data found in t.  Assumes _children is
// empty!
//
// -- implementation ---------------------------------------------------------
// For each branch in t, append a new Tree onto children.
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::copyTree(const Tree<T> &t)
    {
    for (int i = 0 ; i < t._children.length() ; i++)
        _children.append(newNode(*t._children[i]));
    }

// -- private ---------------------------------------------------------------
// template <class T>
// Tree<T>::deleteTree()
//
// Deletes all of the branches in this Tree.
//
// -- implementation ---------------------------------------------------------
// Tell each branch to deleteTree().  Then delete each branch.  Finally,
// set the length of _children to 0.
// ---------------------------------------------------------------------------
template <class T>
void Tree<T>::deleteTree()
    {
    for (int i = 0 ; i < _children.length() ; i++)
        {
        _children[i]->deleteTree();
        delete _children[i];
        }
    _children.setLength(0);
    }

#endif
