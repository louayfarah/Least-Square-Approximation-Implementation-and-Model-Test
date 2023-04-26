//Louay Farah
//l.farah@innopolis.unversity

#include <bits/stdc++.h>

using namespace std;

#define ll long long int
#define pb push_back
#define mp make_pair
#define endl "\n"
#define fi first
#define se second

double epsilon = 1e-7;

template<typename T>
class ColumnVector
{
protected:
    int n;
    vector<T> column;
public:
    ColumnVector()
    {
        n = 0;
    }

    ColumnVector(int n0)
    {
        n = n0;
        column.assign(n, 0);
    }

    friend istream& operator>>(istream& is, ColumnVector<T> &A);
    friend ostream& operator<<(ostream& os, ColumnVector<T> &A);

    int getN()
    {
        return n;
    }

    vector<T>* getColumn()
    {
        return &column;
    }

    void setAtIndex(int i, T v)
    {
        column[i] = v;
    }

    T getAtIndex(int i)
    {
        return column[i];
    }

    ColumnVector operator+(ColumnVector &A)
    {
        if(n != A.getN())
        {
            cout << "Error: the dimensional problem occurred" << endl;
            ColumnVector err(0);
            return err;
        }

        ColumnVector D(n);

        vector<T> *tempA = A.getColumn();
        vector<T> *tempD = D.getColumn();
        for(int i = 0; i<n; i++)
        {
            (*tempD)[i] = column[i] + (*tempA)[i];
        }

        return D;
    }

    ColumnVector operator-(ColumnVector &A)
    {
        if(n != A.getN())
        {
            cout << "Error: the dimensional problem occurred" << endl;
            ColumnVector err(0);
            return err;
        }

        ColumnVector D(n);

        vector<T> *tempA = A.getColumn();
        vector<T> *tempD = D.getColumn();
        for(int i = 0; i<n; i++)
        {
            (*tempD)[i] = column[i] - (*tempA)[i];
        }

        return D;
    }

    double norm()
    {
        double sumOfSquares = 0;
        for(int i = 0; i<n; i++)
        {
            sumOfSquares += column[i]*column[i];
        }

        return sqrt(sumOfSquares);
    }
};

template<typename T>
class Matrix
{
protected:
    int n, m;
    vector<vector<T>> grid;
public:
    Matrix()
    {
        n = 0;
        m = 0;
    }
    Matrix(int n0, int m0)
    {
        n = n0;
        m = m0;

        grid.assign(n, vector<T>(m, 0));
    }

    friend istream& operator>>(istream& is, Matrix<T> &A);
    friend ostream& operator<<(ostream& os, Matrix<T> &A);

    int getN()
    {
        return n;
    }

    int getM()
    {
        return m;
    }

    void setAtIndex(int i, int j, T v)
    {
        grid[i][j] = v;
    }

    vector<vector<T>>* getGrid()
    {
        return &grid;
    }


    bool operator==(Matrix &A)
    {
        if(n != A.getN() || m != A.getM())
        {
            return false;
        }

        vector<vector<T>> *temp = A.getGrid();
        for(int i = 0; i<n; i++)
        {
            for(int j = 0; j<m; j++)
            {
                if(grid[i][j] != (*temp)[i][j])
                    return false;
            }
        }

        return true;
    }

    void operator=(Matrix &A)
    {
        if(n != A.getN() || m != A.getM())
        {
            cout << "Error: the dimensional problem occurred" << endl;
            return;
        }

        vector<vector<T>> *temp = A.getGrid();
        for(int i = 0; i<n; i++)
        {
            for(int j = 0; j<m; j++)
            {
                grid[i][j] = (*temp)[i][j];
            }
        }
    }

    Matrix operator+(Matrix &A)
    {
        if(n != A.getN() || m != A.getM())
        {
            cout << "Error: the dimensional problem occurred" << endl;
            Matrix err(0, 0);
            return err;
        }

        Matrix D(n, m);

        vector<vector<T>> *tempA = A.getGrid();
        vector<vector<T>> *tempD = D.getGrid();
        for(int i = 0; i<n; i++)
        {
            for(int j = 0; j<m; j++)
            {
                (*tempD)[i][j] = grid[i][j] + (*tempA)[i][j];
            }
        }

        return D;
    }

    Matrix operator-(Matrix &A)
    {
        if(n != A.getN() || m != A.getM())
        {
            cout << "Error: the dimensional problem occurred" << endl;
            Matrix err(0, 0);
            return err;
        }

        Matrix E(n, m);

        vector<vector<T>> *tempA = A.getGrid();
        vector<vector<T>> *tempE = E.getGrid();
        for(int i = 0; i<n; i++)
        {
            for(int j = 0; j<m; j++)
            {
                (*tempE)[i][j] = grid[i][j] - (*tempA)[i][j];
            }
        }

        return E;
    }

    Matrix operator*(Matrix &A)
    {
        if(m != A.getN())
        {
            cout << "Error: the dimensional problem occurred" << endl;
            Matrix err(0, 0);
            return err;
        }

        Matrix F(n, A.getM());

        vector<vector<T>> *tempA = A.getGrid();
        vector<vector<T>> *tempF = F.getGrid();

        for(int i = 0; i<n; i++)
        {
            for(int j = 0; j<A.getM(); j++)
            {
                int counter = 0;
                while(counter < m)
                {
                    (*tempF)[i][j] += grid[i][counter]*(*tempA)[counter][j];
                    counter++;
                }
            }
        }

        return F;
    }

    ColumnVector<T> operator*(ColumnVector<T> &A)
    {
        if(m != A.getN())
        {
            cout << "Error: the dimensional problem occurred" << endl;
            ColumnVector<T> err(0);
            return err;
        }

        ColumnVector<T> F(n);

        vector<T> *tempA = A.getColumn();
        vector<T> *tempF = F.getColumn();

        for(int i = 0; i<n; i++)
        {
            int counter = 0;
            while(counter < m)
            {
                (*tempF)[i] += grid[i][counter]*(*tempA)[counter];
                counter++;
            }
        }

        return F;
    }

    Matrix transpose()
    {
        Matrix G(m, n);
        vector<vector<T>> *temp = G.getGrid();

        for(int i = 0; i<n; i++)
        {
            for(int j = 0; j<m; j++)
            {
                (*temp)[j][i] = grid[i][j];
            }
        }

        return G;
    }

    Matrix* augmentedMatrix()
    {
        Matrix* aug = new Matrix(n, 2*m);

        vector<vector<T>> *temp = aug->getGrid();

        for(int i = 0; i<n; i++)
        {
            for(int j = 0; j<m; j++)
            {
                (*temp)[i][j] = grid[i][j];
            }
        }

        for(int i = 0; i<n; i++)
        {
            (*temp)[i][i+m] = 1;
        }

        return aug;
    }
};

template<typename T>
class SquareMatrix: public Matrix<T>
{
public:
    SquareMatrix()
    {
        Matrix<T>::n = 0;
        Matrix<T>::m = 0;
    }

    SquareMatrix(int n0)
    {
        Matrix<T>::n = n0;
        Matrix<T>::m = n0;
        Matrix<T>::grid.assign(Matrix<T>::n, vector<T>(Matrix<T>::n, 0));
    }
};

template<typename T>
class IdentityMatrix: public SquareMatrix<T>
{
public:
    IdentityMatrix()
    {
        Matrix<T>::n = 0;
        Matrix<T>::m = 0;
    }

    IdentityMatrix(int n0)
    {
        Matrix<T>::n = n0;
        Matrix<T>::m = n0;
        Matrix<T>::grid.assign(Matrix<T>::n, vector<T>(Matrix<T>::n, 0));

        for(int i = 0; i<Matrix<T>::n; i++)
        {
            Matrix<T>::grid[i][i] = 1;
        }
    }
};

template<typename T>
class EliminationMatrix: public SquareMatrix<T>
{
public:
    EliminationMatrix()
    {
        Matrix<T>::n = 0;
        Matrix<T>::m = 0;
    }

    EliminationMatrix(int n0)
    {
        Matrix<T>::n = n0;
        Matrix<T>::m = n0;
        Matrix<T>::grid.assign(Matrix<T>::n, vector<T>(Matrix<T>::n, 0));

        for(int i = 0; i<Matrix<T>::n; i++)
        {
            Matrix<T>::grid[i][i] = 1;
        }
    }

    EliminationMatrix(int n0, Matrix<T>* M, int i, int j)
    {
        Matrix<T>::n = n0;
        Matrix<T>::m = n0;
        Matrix<T>::grid.assign(Matrix<T>::n, vector<T>(Matrix<T>::n, 0));

        for(int k = 0; k<Matrix<T>::n; k++)
        {
            Matrix<T>::grid[k][k] = 1;
        }

        vector<vector<T>>* temp = M->getGrid();
        double c = (*temp)[i-1][j-1]/(*temp)[j-1][j-1];

        Matrix<T>::grid[i-1][j-1] = -c;
    }

    void eliminate(Matrix<T>* M, int i, int j)
    {
        vector<vector<T>>* temp = M->getGrid();
        double c = (*temp)[i-1][j-1]/(*temp)[j-1][j-1];

        Matrix<T>::grid[i-1][j-1] = -c;
    }
};

template<typename T>
class PermutationMatrix: public SquareMatrix<T>
{
public:
    PermutationMatrix()
    {
        Matrix<T>::n = 0;
        Matrix<T>::m = 0;
    }

    PermutationMatrix(int n0)
    {
        Matrix<T>::n = n0;
        Matrix<T>::m = n0;
        Matrix<T>::grid.assign(Matrix<T>::n, vector<T>(Matrix<T>::n, 0));

        for(int i = 0; i<Matrix<T>::n; i++)
        {
            Matrix<T>::grid[i][i] = 1;
        }
    }

    PermutationMatrix(int n0, Matrix<T>* M, int i, int j)
    {
        Matrix<T>::n = n0;
        Matrix<T>::m = n0;
        Matrix<T>::grid.assign(Matrix<T>::n, vector<T>(Matrix<T>::n, 0));

        for(int k = 0; k<Matrix<T>::n; k++)
        {
            Matrix<T>::grid[k][k] = 1;
        }

        swap(Matrix<T>::grid[i-1], Matrix<T>::grid[j-1]);
    }

    void permute(Matrix<T>* M, int i, int j)
    {
        swap(Matrix<T>::grid[i-1], Matrix<T>::grid[j-1]);
    }
};

istream& operator>>(istream &is, Matrix<double> *A)
{
    vector<vector<double>> *temp = A->getGrid();
    for(int i = 0; i<A->getN(); i++)
    {
        for(int j = 0; j<A->getM(); j++)
        {
            is >> (*temp)[i][j];
        }
    }

    return is;
}

ostream& operator<<(ostream &os, Matrix<double> &A)
{
    vector<vector<double>> *temp = A.getGrid();
    for(int i = 0; i<A.getN(); i++)
    {
        for(int j = 0; j<A.getM(); j++)
        {
            if(fabs((*temp)[i][j]) < epsilon)
                os << fixed << setprecision(4) << 0.0000;
            else
                os << fixed << setprecision(4) << (*temp)[i][j];
            if(j <A.getM()-1)
            {
                cout << ' ';
            }
        }
        cout << endl;
    }

    return os;
}

istream& operator>>(istream &is, ColumnVector<double> *A)
{
    vector<double> *temp = A->getColumn();
    for(int i = 0; i<A->getN(); i++)
    {
        is >> (*temp)[i];
    }

    return is;
}

ostream& operator<<(ostream &os, ColumnVector<double> &A)
{
    vector<double> *temp = A.getColumn();
    for(int i = 0; i<A.getN(); i++)
    {
        if(fabs((*temp)[i]) < epsilon)
            os << fixed << setprecision(4) << 0.0000;
        else
            os << fixed << setprecision(4) << (*temp)[i];
        cout << endl;
    }

    return os;
}

void showDeterminant(Matrix<double>* A, int n)
{
    int step = 1;
    int nbPermutations = 0;

    for(int j = 1; j<=n-1; j++)
    {
        int r1 = j, r2 = j;
        for(int i = j+1; i<=n; i++)
        {
            double temp1 = (*(A->getGrid()))[r2-1][j-1];
            double temp2 = (*(A->getGrid()))[i-1][j-1];

            if(fabs(temp2) > fabs(temp1))
            {
                r2 = i;
            }
        }

        if(r1 != r2)
        {
            Matrix<double>* P = new PermutationMatrix<double>(n, A, r1, r2);
            Matrix<double> temp = (*P)*(*A);
            *A = temp;

            cout << "step #" << step << ": permutation" << endl;
            cout << *A;

            step++;
            nbPermutations++;
        }

        for(int i = j+1; i<=n; i++)
        {
            Matrix<double>* E = new EliminationMatrix<double>(n, A, i, j);
            Matrix<double> temp = (*E)*(*A);
            *A = temp;

            cout << "step #" << step << ": elimination" << endl;
            cout << *A;

            step++;
        }
    }

    double determinant = 1;
    for(int i = 1; i<=n; i++)
    {
        double temp = (*(A->getGrid()))[i-1][i-1];
        determinant *= temp;
    }

    if(nbPermutations%2 == 1)
    {
        determinant = -determinant;
    }

    cout << "result:" << endl;
    cout << fixed << setprecision(2)<< determinant << endl;
}

Matrix<double> calculateInverse(Matrix<double>* A, int n)
{
    Matrix<double>* aug = A->augmentedMatrix();

    int step = 1;
    for(int j = 1; j<=n-1; j++)
    {
        int r1 = j, r2 = j;
        for(int i = j+1; i<=n; i++)
        {
            double temp1 = (*(A->getGrid()))[r2-1][j-1];
            double temp2 = (*(A->getGrid()))[i-1][j-1];

            if(fabs(temp2) > fabs(temp1))
            {
                r2 = i;
            }
        }

        if(r1 != r2)
        {
            Matrix<double>* P = new PermutationMatrix<double>(n, A, r1, r2);
            Matrix<double> temp = (*P)*(*A);
            *A = temp;

            Matrix<double> temp2 = (*P)*(*aug);
            *aug = temp2;

            step++;
        }

        for(int i = j+1; i<=n; i++)
        {
            Matrix<double>* E = new EliminationMatrix<double>(n, A, i, j);
            Matrix<double> temp = (*E)*(*A);

            if(*A == temp)
            {
                continue;
            }

            *A = temp;

            Matrix<double> temp2 = (*E)*(*aug);
            *aug = temp2;

            step++;
        }
    }


    for(int j = n; j>=2; j--)
    {
        for(int i = j-1; i >= 1; i--)
        {
            Matrix<double>* E = new EliminationMatrix<double>(n, A, i, j);

            Matrix<double> temp = (*E)*(*A);

            if(*A == temp)
            {
                continue;
            }

            *A = temp;

            Matrix<double> temp2 = (*E)*(*aug);
            *aug = temp2;

            step++;
        }
    }

    for(int i = 1; i<=n; i++)
    {
        for(int j = i+1; j<=2*n; j++)
        {
            (*(aug->getGrid()))[i-1][j-1] /= (*(aug->getGrid()))[i-1][i-1];
        }

        (*(aug->getGrid()))[i-1][i-1] = 1;
    }

    for(int i = 1; i<=n; i++)
    {
        for(int j = 1; j<=n; j++)
        {
            (*(A->getGrid()))[i-1][j-1] = (*(aug->getGrid()))[i-1][j-1+n];
        }
    }

    return *A;
}

void solveSystem(Matrix<double>* A, int n, ColumnVector<double>* b, int m)
{
    cout << "step #" << 0 << ':' << endl;
    cout << *A;
    cout << *b;


    int step = 1;
    for(int j = 1; j<=n-1; j++)
    {
        int r1 = j, r2 = j;
        for(int i = j+1; i<=n; i++)
        {
            double temp1 = (*(A->getGrid()))[r2-1][j-1];
            double temp2 = (*(A->getGrid()))[i-1][j-1];

            if(fabs(temp2) > fabs(temp1))
            {
                r2 = i;
            }
        }

        if(r1 != r2)
        {
            Matrix<double>* P = new PermutationMatrix<double>(n, A, r1, r2);
            Matrix<double> temp = (*P)*(*A);
            *A = temp;

            ColumnVector<double> temp2 = (*P)*(*b);
            *b = temp2;

            cout << "step #" << step << ": permutation" << endl;
            cout << *A;
            cout << *b;

            step++;
        }

        for(int i = j+1; i<=n; i++)
        {
            Matrix<double>* E = new EliminationMatrix<double>(n, A, i, j);
            Matrix<double> temp = (*E)*(*A);

            if(*A == temp)
            {
                continue;
            }

            *A = temp;

            ColumnVector<double> temp2 = (*E)*(*b);
            *b = temp2;

            cout << "step #" << step << ": elimination" << endl;
            cout << *A;
            cout << *b;

            step++;
        }
    }

    for(int j = n; j>=2; j--)
    {
        for(int i = j-1; i >= 1; i--)
        {
            Matrix<double>* E = new EliminationMatrix<double>(n, A, i, j);

            Matrix<double> temp = (*E)*(*A);

            if(*A == temp)
            {
                continue;
            }

            *A = temp;

            ColumnVector<double> temp2 = (*E)*(*b);
            *b = temp2;

            cout << "step #" << step << ": elimination" << endl;
            cout << *A;
            cout << *b;

            step++;
        }
    }

    cout << "Diagonal normalization:" << endl;
    for(int i = 1; i<=n; i++)
    {
        if((*(A->getGrid()))[i-1][i-1] != 0)
        {
            (*(b->getColumn()))[i-1] /= (*(A->getGrid()))[i-1][i-1];
            (*(A->getGrid()))[i-1][i-1] = 1;
        }
    }
    cout << *A;
    cout << *b;

    cout << "result:" << endl;
    cout << *b;
}

ColumnVector<double> leastSquaresApproximation(ColumnVector<double> *t, ColumnVector<double> *b, int m, Matrix<double> *A, int n)
{
    cout << fixed << setprecision(4) << "A:" << endl << *A;

    Matrix<double> B = (A->transpose())*(*A);
    cout << "A_T*A:" << endl << B;

    Matrix<double> C = calculateInverse(&B, n+1);
    cout << "(A_T*A)^-1:" << endl << C;

    ColumnVector<double> D = (A->transpose())*(*b);
    cout << "A_T*b:" << endl << D;

    ColumnVector<double> x = C*D;
    cout << "x~:" << endl << x;

    return x;
}

#ifdef WIN32
#define GNUPLOT_NAME "C:\\gnuplot\\bin\\gnuplot -persist"
#else
#define GNUPLOT_NAME "gnuplot -persist"
#endif

int main()
{
    #ifdef WIN32
        FILE* pipe = _popen(GNUPLOT_NAME, "w");
    #else
        FILE* pipe = popen(GNUPLOT_NAME, "w");
    #endif

    int m;
    cin >> m;

    /*
    The set of m = 15 points is:
    -9 -5
    -9 3
    -7 -8
    -5 -5
    -3 -4
    -3 2
    -1 2
    -1 5
    2 5
    2 9
    3 -4
    5 6
    5 7
    8 9
    9 10
    */

    ColumnVector<double> *t = new ColumnVector<double>(m);
    ColumnVector<double> *b = new ColumnVector<double>(m);
    for(int i = 0; i<m; i++)
    {
        double x;
        cin >> x;
        t->setAtIndex(i, x);

        cin >> x;
        b->setAtIndex(i, x);
    }

    int n;
    cin >> n;

    Matrix<double> *A = new Matrix<double>(m, n+1);
    for(int i = 0; i<m; i++)
    {
        for(int j = 0; j<n+1; j++)
        {
            A->setAtIndex(i, j, pow((*(t->getColumn()))[i], j));
        }
    }

    ColumnVector<double> x = leastSquaresApproximation(t, b, m, A, n);

    fprintf(pipe, "plot [-10 : 10] [-10 : 10] %lf*x**4 + %lf*x**3 + %lf*x**2 + %lf*x**1 + %lf*x**0 , '-' using 1:2 with points\n", x.getAtIndex(4), x.getAtIndex(3), x.getAtIndex(2), x.getAtIndex(1), x.getAtIndex(0));

    for(int i = 0; i<m; i++)
    {
        fprintf(pipe, "%f\t%f\n", t->getAtIndex(i), b->getAtIndex(i));
    }


    fprintf(pipe, "e\n");
    fflush(pipe);


    #ifdef WIN32
        _pclose(pipe);
    #else
        pclose(pipe);
    #endif
    return 0;
}
