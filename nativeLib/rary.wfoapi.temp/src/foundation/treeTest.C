#include "Tree.H"

class Test
    {
    public:
        Test() : _x(0) {}
        Test(int x) : _x(x) {}

        std::ostream &printOn(std::ostream &os) const
            { return os << _x; }

        bool operator==(const Test &rhs) const { return _x == rhs._x; }
        bool operator!=(const Test &rhs) const { return _x != rhs._x; }
        
        bool operator<(const Test &rhs) const { return _x < rhs._x; }
        bool operator>(const Test &rhs) const { return _x > rhs._x; }

    private:
        int _x;
    };

std::ostream &operator<<(std::ostream &os, const Test &t)
    {
    return t.printOn(os);
    }


int main()
    {
    Tree<Test> tree(5);

    tree.append(6);
    tree.append(4);
    tree[0].append(2);

    std::cout << tree << std::endl;

    return 0;
    }
