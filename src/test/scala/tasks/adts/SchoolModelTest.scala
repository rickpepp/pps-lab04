package tasks.adts

import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.Test
import tasks.adts.SchoolModel.*
import u03.extensionmethods.Sequences.*


class SchoolModelTest:
  val schoolModel: SchoolModule = BasicSchoolModule
  import schoolModel.*

  @Test def testTeacher(): Unit =
    assertEquals("John", teacher("John"))

  @Test def testCourse(): Unit =
    assertEquals("Math", course("Math"))

  @Test def testEmptySchool(): Unit =
    assertEquals(Sequence.nil(), emptySchool.courses)
    assertEquals(Sequence.nil(), emptySchool.teachers)
    assertFalse(emptySchool.hasTeacher("John"))
    assertFalse(emptySchool.hasCourse("Math"))
    
  @Test def testSetTeacherToCourse(): Unit =
    val school2 = emptySchool.setTeacherToCourse(teacher("John"), course("Math"))
    println(school2.teachers) // Cons("John", Nil())
    println(school2.courses) // Cons("Math", Nil())
    println(school2.hasTeacher("John")) // true
    println(school2.hasCourse("Math")) // true
    println(school2.hasCourse("Italian")) // false