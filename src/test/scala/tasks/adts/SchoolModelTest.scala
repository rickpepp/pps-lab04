package tasks.adts

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import tasks.adts.SchoolModel.*
import u03.extensionmethods.Sequences.{Sequence, *}


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
    assertEquals(Sequence.Cons("John", Sequence.Nil()),school2.teachers)
    assertEquals(Sequence.Cons("Math", Sequence.Nil()),school2.courses)
    assertTrue(school2.hasTeacher("John"))
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school2.hasCourse("Italian"))