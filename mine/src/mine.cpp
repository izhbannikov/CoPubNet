#include <stdio.h>
#include <string.h>
#include <iostream>
#include <algorithm>
#include <vector>
#include <Rcpp.h> 

using namespace Rcpp;
using namespace std;

//void replaceAll(std::string& str, const std::string& from, const std::string& to);
//bool replace(std::string& str, const std::string& from, const std::string& to);
//void stoupper(std::string& s);
//vector<int> process_text(string text, vector<string> keywords);


bool replace(std::string& str, const std::string& from, const std::string& to) {
    size_t start_pos = str.find(from);
    if(start_pos == std::string::npos)
        return false;
    str.replace(start_pos, from.length(), to);
    return true;
}

void replaceAll(std::string& str, const std::string& from, const std::string& to) {
    if(from.empty())
        return;
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
}

//Convert string to upper case
void stoupper(std::string& s)	{
        std::string::iterator i = s.begin();
        std::string::iterator end = s.end();

        while (i != end) {
                *i = std::toupper((unsigned char)*i);
                ++i;
        }
}

RcppExport SEXP process_text(SEXP text, SEXP kwords) {
//vector<int> process_text(string text, vector<string> keywords) {
    string txt = as<string>(text);
    Rcpp::CharacterVector cx = Rcpp::CharacterVector(kwords);  
    vector<string> keywords;
    for (int i=0; i<cx.size(); i++) 
    {  
      keywords.push_back(string(cx[i]));  
    } 
    
    stoupper(txt);
    vector<int> out;//, vector<string> keywords
    //find dots and other non-needed characters and replace them by dots:
    std::replace( txt.begin(), txt.end(), '.', '\0');
    std::replace( txt.begin(), txt.end(), ' ', '\0');
    //cout << keywords.size() << endl;
    
    for(unsigned short i = 0; i<keywords.size(); ++i) {
        string keyword = keywords[i];
        stoupper(keyword);
        std::replace( keyword.begin(), keyword.end(), '.', '\0');
        std::replace( keyword.begin(), keyword.end(), ' ', '\0');
        
        int n = 0;
        std::string ::size_type pos = 0;
        while( (pos = txt.find( keyword, pos )) != std::string::npos ) {
           n++;
           pos += 1;
	}
        //cout << keywords[i] << " was found " << n << " times\n";
        out.push_back(n);
    }
    
    return(Rcpp::wrap(out));
    //return (R_NilValue);
}
