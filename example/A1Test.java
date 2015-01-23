import static org.junit.Assert.*;

import org.junit.BeforeClass;
import org.junit.Test;

import se2s03.A1;

public class A1Test {
    
    private static A1 tester;
    
    @BeforeClass
    public static void setUp(){
        tester = new A1();
    }
    
    @Test
    public void casesTest() {
        assertEquals("cases(0,0,0) must be 10", 10, tester.cases(0,0,0));
        assertEquals("cases(4,0,0) must be 6", 6, tester.cases(4,0,0));
        assertEquals("cases(3,-5,12) must be 7", 7, tester.cases(3,-5,12));
        assertEquals("cases(-1,-6,13) must be 9", 9, tester.cases(-1,-6,13));
        assertEquals("cases(2,-4,4) must be 10", 10, tester.cases(2,-4,4));
        assertEquals("cases(2,-4,5) must be 1", 1, tester.cases(2,-4,5));
        assertEquals("cases(4,9,2) must be 6", 6, tester.cases(4,9,2));
        assertEquals("cases(6,10,-7) must be 3", 3, tester.cases(6,10,-7));
        assertEquals("cases(3,-3,1) must be 10", 10, tester.cases(3,-3,1));
        assertEquals("cases(12,-6,3) must be 5", 5, tester.cases(12,-6,3));
        assertEquals("cases(12,-5,3) must be 2", 2, tester.cases(12,-5,3));
        assertEquals("cases(-100,-10,11) must be 7", 7, tester.cases(-100,-10,11));
        assertEquals("cases(1,-1,0) must be 10", 10, tester.cases(1,-1,0));
        assertEquals("cases(16,2,-9) must be 6", 6, tester.cases(16,2,-9));
        assertEquals("cases(-7,6,2) must be 10", 10, tester.cases(-7,6,2));
        assertEquals("cases(-10,-2,9) must be 1", 1, tester.cases(-10,-2,9));
        assertEquals("cases(11,7,6) must be 2", 2, tester.cases(11,7,6));
        assertEquals("cases(-8,2,-5) must be 10", 10, tester.cases(-8,2,-5));
        assertEquals("cases(4,0,1) must be 6", 6, tester.cases(4,0,1));
        assertEquals("cases(1,0,-4) must be 10", 10, tester.cases(1,0,-4));
        assertEquals("cases(9,2,-9) must be 6", 6, tester.cases(9,2,-9));
        assertEquals("cases(3,-5,17) must be 9", 9, tester.cases(3,-5,17));
        assertEquals("cases(-1,-4,-1) must be 10", 10, tester.cases(-1,-4,-1));
        assertEquals("cases(2,-4,13) must be 1", 1, tester.cases(2,-4,13));
        assertEquals("cases(18,-5,7) must be 2", 2, tester.cases(18,-5,7));
        assertEquals("cases(-7,-15,5) must be 7", 7, tester.cases(-7,-15,5));
        assertEquals("cases(7,-7,3) must be 5", 5, tester.cases(7,-7,3));
        assertEquals("cases(4,9,-10) must be 6", 6, tester.cases(4,9,-10));
        assertEquals("cases(6,10,-15) must be 3", 3, tester.cases(6,10,-15));
        assertEquals("cases(13,-7,30) must be 5", 5, tester.cases(13,-7,30));
        assertEquals("cases(0,10,0) must be 10", 10, tester.cases(0,10,0));
        assertEquals("cases(5,-15,1) must be 6", 6, tester.cases(5,-15,1));
        assertEquals("cases(2,0,5) must be 1", 1, tester.cases(2,0,5));
        assertEquals("cases(1,-4,1) must be 10", 10, tester.cases(1,-4,1));
        assertEquals("cases(3,-6,2) must be 7", 7, tester.cases(3,-6,2));
        assertEquals("cases(0,0,5) must be 1", 1, tester.cases(0,0,5));
        assertEquals("cases(10,0,-5) must be 6", 6, tester.cases(10,0,-5));
        assertEquals("cases(20,-18,10) must be 5", 5, tester.cases(20,-18,10));
        assertEquals("cases(2,-7,1) must be 7", 7, tester.cases(2,-7,1));
        assertEquals("cases(4,-5,3) must be 2", 2, tester.cases(4,-5,3));
        assertEquals("cases(3,-3,5) must be 1", 1, tester.cases(3,-3,5));
        assertEquals("cases(4,8,1) must be 6", 6, tester.cases(4,8,1));
        assertEquals("cases(2,9,2) must be 10", 10, tester.cases(2,9,2));
        assertEquals("cases(3,10,3) must be 10", 10, tester.cases(3,10,3));
        assertEquals("cases(4,-7,4) must be 5", 5, tester.cases(4,-7,4));
        assertEquals("cases(2,-6,5) must be 7", 7, tester.cases(2,-6,5));
        assertEquals("cases(3,-5,1) must be 7", 7, tester.cases(3,-5,1));
        assertEquals("cases(4,-4,2) must be 6", 6, tester.cases(4,-4,2));
        assertEquals("cases(2,-3,3) must be 10", 10, tester.cases(2,-3,3));
        assertEquals("cases(3,8,4) must be 10", 10, tester.cases(3,8,4));
        assertEquals("cases(4,9,5) must be 2", 2, tester.cases(4,9,5));
        assertEquals("cases(2,10,1) must be 10", 10, tester.cases(2,10,1));
        assertEquals("cases(3,-7,2) must be 7", 7, tester.cases(3,-7,2));
        assertEquals("cases(4,-6,3) must be 5", 5, tester.cases(4,-6,3));
        assertEquals("cases(2,-5,4) must be 7", 7, tester.cases(2,-5,4));
        assertEquals("cases(3,-4,5) must be 1", 1, tester.cases(3,-4,5));
        assertEquals("cases(4,-3,1) must be 6", 6, tester.cases(4,-3,1));
        assertEquals("cases(2,8,2) must be 10", 10, tester.cases(2,8,2));
        assertEquals("cases(3,-5,13) must be 9", 9, tester.cases(3,-5,13));
        assertEquals("cases(3,9,3) must be 10", 10, tester.cases(3,9,3));
        assertEquals("cases(4,10,4) must be 2", 2, tester.cases(4,10,4));
        assertEquals("cases(2,-7,5) must be 7", 7, tester.cases(2,-7,5));
        assertEquals("cases(2,9,1) must be 10", 10, tester.cases(2,9,1));
        assertEquals("cases(3,-6,1) must be 7", 7, tester.cases(3,-6,1));
        assertEquals("cases(4,-5,2) must be 6", 6, tester.cases(4,-5,2));
        assertEquals("cases(2,-4,3) must be 10", 10, tester.cases(2,-4,3));
        assertEquals("cases(3,-3,4) must be 10", 10, tester.cases(3,-3,4));
        assertEquals("cases(4,8,5) must be 2", 2, tester.cases(4,8,5));
        assertEquals("cases(3,10,2) must be 10", 10, tester.cases(3,10,2));
        assertEquals("cases(4,10,2) must be 3", 3, tester.cases(4,10,2));
        assertEquals("cases(4,9,1) must be 6", 6, tester.cases(4,9,1));
        assertEquals("cases(2,-6,13) must be 9", 9, tester.cases(2,-6,13));
    }
}
