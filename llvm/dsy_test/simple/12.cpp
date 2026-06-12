// Test 12: switch-like if-else chain
#include "../top.h"
void top () {
    int score, grade;
    registerInput("score", &score, sizeof(score));

    if (score >= 90)
        grade = 4;
    else if (score >= 80)
        grade = 3;
    else if (score >= 70)
        grade = 2;
    else if (score >= 60)
        grade = 1;
    else
        grade = 0;

    registerOutput("grade", &grade, sizeof(grade));
}
