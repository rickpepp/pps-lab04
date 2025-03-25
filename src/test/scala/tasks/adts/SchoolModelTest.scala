package tasks.adts

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import tasks.adts.SchoolModel.*
import u03.extensionmethods.Sequences.{Sequence, *}


class SchoolModelTest:
  import schoolModel.*
  val schoolModel: SchoolModule = BasicSchoolModule
  val john: Teacher = teacher("John")
  val mark: Teacher = teacher("Mark")
  val math: Course = course("Math")
  val italian: Course = course("Italian")

  @Test def testTeacher(): Unit =
    assertEquals("John", john)

  @Test def testCourse(): Unit =
    assertEquals("Math", math)

  @Test def testEmptySchool(): Unit =
    assertEquals(Sequence.nil(), emptySchool.courses)
    assertEquals(Sequence.nil(), emptySchool.teachers)
    assertFalse(emptySchool.hasTeacher("John"))
    assertFalse(emptySchool.hasCourse("Math"))
    
  @Test def testSetTeacherToCourse(): Unit =
    val school = emptySchool.setTeacherToCourse(john, math)
    assertEquals(Sequence.Cons("John", Sequence.Nil()),school.teachers)
    assertEquals(Sequence.Cons("Math", Sequence.Nil()),school.courses)
    assertTrue(school.hasTeacher("John"))
    assertTrue(school.hasCourse("Math"))
    assertFalse(school.hasCourse("Italian"))

  @Test def testCoursesOfTeachersValues(): Unit =
    val school = emptySchool.setTeacherToCourse(john, math)
      .setTeacherToCourse(john, italian)
    assertEquals(Sequence.Cons("Italian", Sequence.Cons("Math", Sequence.Nil())), school.coursesOfATeacher(john))

  @Test def testDuplicateCoursesValues(): Unit =
    val school = emptySchool.setTeacherToCourse(john, math)
      .setTeacherToCourse(mark, math)
    assertEquals(Sequence.Cons("Math", Sequence.Nil()), school.courses)
    assertEquals(Sequence.Cons("Mark", Sequence.Cons("John", Sequence.Nil())), school.teachers)

  @Test def testDuplicateTeachersValues(): Unit =
    val school = emptySchool.setTeacherToCourse(john, math)
      .setTeacherToCourse(john, italian)
    assertEquals(Sequence.Cons("John", Sequence.Nil()), school.teachers)