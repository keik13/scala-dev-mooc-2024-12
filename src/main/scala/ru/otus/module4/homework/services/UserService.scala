package ru.otus.module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.{ZIO, ZLayer}

import java.sql.SQLException

trait UserService{
    def listUsers(): QIO[List[User]]
    def listUsersDTO(): QIO[List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}
case class Impl(userRepo: UserRepository) extends UserService {
    val ctx = db.Ctx

    def listUsers(): QIO[List[User]] =
        userRepo.list()


    def listUsersDTO(): QIO[List[UserDTO]] = for {
      users <- listUsers()
      roles <- ZIO.foreach(users){u => userRepo.userRoles(u.typedId)}
    } yield users.zip(roles).map{ case (user, roles) => UserDTO(user, roles.toSet)}

    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] = ctx.transaction(for {
      user <- userRepo.createUser(user)
      roles <- userRepo.userRoles(user.typedId)
      _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
    } yield UserDTO(user, roles.toSet))
      .catchAll(e => throw new SQLException(e.getMessage))

    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] = for {
      users <- userRepo.listUsersWithRole(roleCode)
      roles <- ZIO.foreach(users){u => userRepo.userRoles(u.typedId)}
    } yield users.zip(roles).map{case (user, roles) => UserDTO(user, roles.toSet)}


}
object UserService{

    val layer: ZLayer[UserRepository, Nothing, UserService] = ZLayer.fromFunction(Impl.apply _)
}

case class UserDTO(user: User, roles: Set[Role])