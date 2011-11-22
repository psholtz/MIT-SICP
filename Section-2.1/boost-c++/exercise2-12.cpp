//
//  main.cpp
//  SICP-Interval3
//
//  Created by Paul Sholtz on 11/21/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include <iostream>

#include <boost/numeric/interval.hpp>

using namespace std;
using namespace boost::numeric;

// procedure declarations
interval<float> make_center_width(float center, float width);
interval<float> make_center_percent(float center, float percent);

float center(interval<float> p);
float width(interval<float> p);
float percent(interval<float> p);

// procedure definitions
interval<float> make_center_width(float center, float width) {
    return interval<float>(center-width,center+width);
}
interval<float> make_center_percent(float center, float percent) {
    return make_center_width(center, center*percent);
}

float center(interval<float> p) {
    return (p.lower() + p.upper()) / 2.0f;
}
float width(interval<float> p) {
    return (p.upper() - p.lower()) / 2.0f;
}
float percent(interval<float> p) {
    return width(p) / center(p); 
}

/*********************************************************************
 * The output from this program is the following, which matches the 
 * reference Scheme implementation:
 *
 
 3.5
 0.15
 3.35
 3.65
 
 3
 0.1
 0.3
 2.7
 3.3
 
 ********************************************************************/
int main (int argc, const char * argv[])
{
    // test the "center" code 
    interval<float> x = make_center_width(3.5f, 0.15f);
    cout << center(x) << endl;
    cout << width(x) << endl;
    cout << x.lower() << endl;
    cout << x.upper() << endl; 
    cout << endl;
    
    // test the "percent" code
    interval<float> y = make_center_percent(3.0f, 0.10f);
    cout << center(y) << endl;
    cout << percent(y) << endl;
    cout << width(y) << endl;
    cout << y.lower() << endl;
    cout << y.upper() << endl; 
    cout << endl; 

    return 0;
}

