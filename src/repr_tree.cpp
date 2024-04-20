#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

List variableColumnList(int numColumns = 30, int numRows = 100)
{
    List retval;
    for (int i = 0; i < numColumns; i++)
    {
        std::ostringstream colName;
        colName << "X" << i + 1;
        NumericVector v(numRows);
        retval.push_back(v, colName.str());
    }
    return retval;
}

int pow2(int exponent)
{

    if (exponent <= 0)
    {
        return 1;
    }
    else
    {
        int val = 2;
        for (int i = 1; i < exponent; i++)
        {
            val *= 2;
        }
        return val;
    }
}

DataFrame variableColumnListAsDF(int numColumns = 30, int numRows = 100)
{
    Function asDF("as.data.frame");

    return asDF(variableColumnList(numColumns, numRows));
}

// [[Rcpp::export]]
DataFrame repr_tree_cpp(DataFrame data, DataFrame is_na,
                        IntegerVector roots,
                        IntegerVector yes,
                        IntegerVector no,
                        IntegerVector missing,
                        LogicalVector is_leaf,
                        IntegerVector feature,
                        NumericVector split,
                        IntegerVector decision_type,
                        IntegerVector trees,
                        IntegerVector depth)
{

    int max_depth = depth[0];
    int n = data.ncol();
    int n_trees = roots.size();

    DataFrame out = variableColumnListAsDF(n_trees, n);

    for (int i = 0; i < n; ++i)
    {
        NumericVector observation = data[i];
        LogicalVector observation_is_na = is_na[i];

        for (int j = 0; j < n_trees; ++j)
        {
            // Rcout << "i=" << i << " j=" << j << std::endl;
            int node = roots[j];
            int depth = 0;
            int embedding = 1;

            while (!is_leaf[node] && depth < max_depth)
            {

                if (((decision_type[node] == 1) && (observation[feature[node]] <= split[node])) ||
                    ((decision_type[node] == 2) && (observation[feature[node]] < split[node])) ||
                    ((observation_is_na[feature[node]]) && (missing[node] == yes[node])))
                {

                    embedding *= 2;
                    depth += 1;
                    node = yes[node];

                    // NumericVector tmp_col = out[j];
                    // double tmp_val = tmp_col[i];
                    // tmp_col[i] = tmp_val * 2;
                    // // Rcout << tmp_col[i] << " ";
                    // node = yes[node];
                }
                else
                {
                    embedding *= 2;
                    embedding++;
                    depth++;
                    node = no[node];

                    // NumericVector tmp_col = out[j];
                    // double tmp_val = tmp_col[i];
                    // tmp_col[i] = tmp_val * 2 + 1;
                    // // Rcout << tmp_col[i] << " ";
                    // node = no[node];
                }
            }
            // Rcout << std::endl;
            NumericVector v = out[j];
            v[i] = embedding * pow2(max_depth - depth);

        }

    }
    return out;
}