//
//  main.cpp
//  SICP-Interval4
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

interval<float> par1(interval<float> r1,interval<float> r2);
interval<float> par2(interval<float> r1,interval<float> r2);

void display(interval<float> p);

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

interval<float> par1(interval<float> r1,interval<float> r2) {
    return (r1*r2) / (r1+r2);
}

interval<float> par2(interval<float> r1,interval<float> r2) {
    interval<float> one = interval<float>(1.0f,1.0f);
    return one / ((one/r1) + (one/r2));
}

void display(interval<float> p) {
    cout << "(" << p.lower() << "," << p.upper() << ")" << endl;
}

/*********************************************************************** 
 * The output of this program is the following, which corresponds with 
 * the results obtained from the reference Scheme implementation:
 *
 (0.9,1.1)
 (1.8,2.2)
 
 (0.490909,0.896297)
 (0.6,0.733333)
 
 0.693603
 0.666667
 
 0.202694
 0.0666667
 
 0.292233
 0.1
 
 (39800,40200)
 (64187.5,65812.5)
 
 (24097.7,25442.1)
 (24567,24956.1)
 
 24769.9
 24761.6
 
 0.0271373
 0.00785751
 
 (0.99005,1.01005)
 (0.975309,1.02532)
 
 1.00005
 1.00031
 
 0.00999979
 0.0249961
 
 0.615519
 1.62514
 
 (7,13)
 (6,14)
 
 10
 10
 
 (1.55556,14)
 (3.23077,6.74074)
 
 7.77778
 4.98575
 1
 
 (0.538462,1.85714)
 (0.428571,2.33333)
 
 1.1978
 1.38095
 
 0.550459
 0.689655
 
 1.33333
 1.23077
 
 0.625
 0.625
 *********************************************************************/
int main (int argc, const char * argv[])
{
    // USE CASES I 
    interval<float> x = make_center_percent(1.0f, 0.1f);
    interval<float> y = make_center_percent(2.0f, 0.1f);
    display(x);
    display(y);
    cout << endl; 
  
    display(par1(x,y));
    display(par2(x,y));
    cout << endl;
    
    cout << center(par1(x,y)) << endl;
    cout << center(par2(x,y)) << endl;
    cout << endl;
    
    cout << width(par1(x,y)) << endl;
    cout << width(par2(x,y)) << endl; 
    cout << endl;
    
    cout << percent(par1(x,y)) << endl;
    cout << percent(par2(x,y)) << endl;
    cout << endl;
    
    // USE CASES II 
    x = make_center_percent(40000, 0.005);
    y = make_center_percent(65000, 0.0125);
    display(x);
    display(y);
    cout << endl;
    
    display(par1(x,y));
    display(par2(x,y));
    cout << endl;
    
    cout << center(par1(x,y)) << endl;
    cout << center(par2(x,y)) << endl; 
    cout << endl;
    
    cout << percent(par1(x,y)) << endl;
    cout << percent(par2(x,y)) << endl;
    cout << endl; 
    
    display(x/x);
    display(y/y);
    cout << endl;
    
    cout << center(x/x) << endl;
    cout << center(y/y) << endl;
    cout << endl;
    
    cout << percent(x/x) << endl;
    cout << percent(y/y) << endl;
    cout << endl; 
    
    cout << center(x/y) << endl;
    cout << center(y/x) << endl; 
    cout << endl; 
    
    // USE CASES III
    x = make_center_percent(10.0f, 0.3f);
    y = make_center_percent(10.0f, 0.4f);
    display(x);
    display(y);
    cout << endl;
    
    cout << center(x) << endl;
    cout << center(y) << endl;
    cout << endl;
    
    display(par1(x,y));
    display(par2(x,y));
    cout << endl;
    
    cout << center(par1(x,y)) << endl;
    cout << center(par2(x,y)) << endl;
    cout << (center(par1(x,y)) > (par2(x,y)).upper()) << endl;
    cout << endl;
    
    display(x/x);
    display(y/y);
    cout << endl;
    
    cout << center(x/x) << endl;
    cout << center(y/y) << endl;
    cout << endl;
    
    cout << percent(x/x) << endl;
    cout << percent(y/y) << endl;
    cout << endl;
    
    cout << center(x/y) << endl;
    cout << center(y/x) << endl; 
    cout << endl; 
    
    cout << percent(x/y) << endl;
    cout << percent(y/x) << endl;
    cout << endl; 
    
    return 0;
}

