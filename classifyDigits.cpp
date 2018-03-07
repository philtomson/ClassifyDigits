#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <numeric>
#include <limits>
#include <boost/algorithm/string.hpp>
#include <math.h>
/* C++ version
   compile with( clang++-5.0 required):
   clang++-5.0 -o classifyDigits classifyDigits.cpp -std=c++1z -O3

   -or-

   g++ -o classifyDigits classifyDigits.cpp -std=c++1z -O3
*/

/*
// This F# dojo is directly inspired by the 
// Digit Recognizer competition from Kaggle.com:
// http://www.kaggle.com/c/digit-recognizer
// The datasets below are simply shorter versions of
// the training dataset from Kaggle.
 
// The goal of the dojo will be to
// create a classifier that uses training data
// to recognize hand-written digits, and
// evaluate the quality of our classifier
// by looking at predictions on the validation data.

*/

bool read_lines(std::string& fileName, std::vector<std::string>& vecOfStrs)
{
	// Open the File
	std::ifstream in(fileName.c_str());
	// Check if object is valid
	if(!in)
	{
		std::cerr << "Cannot open the File : "<<fileName<<std::endl;
		return false;
	}
	std::string str;
	// Read the next line from File untill it reaches the end.
	while (std::getline(in, str))
	{
		// Line contains string of length > 0 then save it in vector
		if(str.size() > 0)
			vecOfStrs.push_back(str);
	}
	//Close The File
	in.close();
	return true;
}

  
// Two data files are included in the same place you
// found this script: 
// trainingsample.csv, a file that contains 5,000 examples, and 
// validationsample.csv, a file that contains 500 examples.
// The first file will be used to train your model, and the
// second one to validate the quality of the model.
 

struct LabelPixels {
   int label;
   std::vector<int> pixels;
};

std::vector<LabelPixels> slurp_file(const char* fname){
   //std::cout << "slurp_file(" << fname << ")\n";
   std::string fn(fname);
   std::vector<std::string> lines;
   std::vector<LabelPixels> labels_pixels;
   if(!read_lines(fn, lines)) {
      std::cout << "!!Can't open file: " << fn << "!!\n";
   }
   //get rid of the first line with lable titles:
   lines.erase(lines.begin());

   for(auto& line : lines){
      int label = (line.front())-'0';//TODO: all of the labels are 1 char
                                     //otherwise this would be
                                     //a bad idea!!
      std::vector<int> pixels ;
      //split the line by ','
      std::vector<std::string> pix_strs;
      boost::split(pix_strs, line, [](char c){ return c == ',';});
      
      std::transform(pix_strs.begin()+1,
                    pix_strs.end(),
                    std::back_inserter(pixels),
                    [](std::string s) { return std::stoi(s);});

      labels_pixels.push_back(LabelPixels{.label  = label,
                                          .pixels = pixels });
      
   }
   return labels_pixels;
}

// 6. COMPUTING DISTANCES
 
// We need to compute the distance between images
// Math reminder: the euclidean distance is
// distance [ x1; y1; z1 ] [ x2; y2; z2 ] = 
// sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2))

double distance(std::vector<int>& p1, std::vector<int>& p2){
   std::vector<int> diff_list;
   std::transform(p1.begin(), p1.end(), p2.begin(), 
                  std::back_inserter(diff_list),
                  [](int a, int b) {return (a-b)*(a-b);});
   int sum = std::accumulate(diff_list.begin(), diff_list.end(), 0);
   return sqrt(sum);
}

// 7. WRITING THE CLASSIFIER FUNCTION
 
// We are now ready to write a classifier function!
// The classifier should take a set of pixels
// (an array of ints) as an input, search for the
// closest example in our sample, and predict
// the value of that closest element.

int classify(std::vector<int>& pixels, std::vector<LabelPixels>& samples){
   int min_dist = std::numeric_limits<int>::max();
   int result=-1; //if -1 gets returned the result is invalid
   for(auto& x : samples){
      int dist = distance(pixels, x.pixels);
      if(dist < min_dist){
         min_dist = dist;
         result   = x.label;
      }
   }
   return result;
}
 
// 8. EVALUATING THE MODEL AGAINST VALIDATION DATA
 
// Now that we have a classifier, we need to check
// how good it is. 
// This is where the 2nd file, validationsample.csv,
// comes in handy. 
// For each Example in the 2nd file,
// we know what the true Label is, so we can compare
// that value with what the classifier says.
// You could now check for each 500 example in that file
// whether your classifier returns the correct answer,
// and compute the % correctly predicted.

int main(){
   auto trainingsample = slurp_file("./trainingsample.csv");
   auto validationsample = slurp_file("./validationsample.csv");
   //std::cout << "validation sample size: " << validationsample.size() << std::endl;
   int num_correct = 0;
   for(auto& p : validationsample){
      if(classify(p.pixels, trainingsample) == p.label){
         num_correct++;
      }
   }
   float percent_correct = (float)num_correct/(float)validationsample.size();
   std::cout << "Percentage correct: " << percent_correct*100.0 << std::endl;
   return 0;
}

