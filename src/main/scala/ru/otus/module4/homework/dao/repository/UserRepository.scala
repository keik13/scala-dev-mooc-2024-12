package ru.otus.module4.homework.dao.repository

import zio.{ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource

trait UserRepository{
    def findUser(userId: UserId): QIO[Option[User]]
    def createUser(user: User): QIO[User]
    def createUsers(users: List[User]): QIO[List[User]]
    def updateUser(user: User): QIO[Unit]
    def deleteUser(user: User): QIO[Unit]
    def findByLastName(lastName: String): QIO[List[User]]
    def list(): QIO[List[User]]
    def userRoles(userId: UserId): QIO[List[Role]]
    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
    def createRole(role: Role): QIO[Role]
}


class UserRepositoryImpl extends UserRepository {
    val ctx = db.Ctx
    import ctx._

    val userSchema = quote{querySchema[User]("""user_table""")}
    val roleSchema = quote{querySchema[Role]("""role""")}
    val userToRoleSchema = quote{querySchema[UserToRole]("""user_to_role""")}

    override def findUser(userId: UserId): QIO[Option[User]] = ctx.run(userSchema.filter(_.id == lift(userId.id))).map(_.headOption)

    override def createUser(user: User): QIO[User] = ctx.run(userSchema.insertValue(lift(user)).returning(u => u))

    override def createUsers(users: List[User]): QIO[List[User]] = ctx.run(liftQuery(users).foreach(us => userSchema.insertValue(us).returning(u => u)))

    override def updateUser(user: User): QIO[Unit] =  ctx.run(userSchema.filter(_.id == lift(user.id)).updateValue(lift(user))).unit

    override def deleteUser(user: User): QIO[Unit] = ctx.run(userSchema.filter(_.id == lift(user.id)).delete).unit

    override def findByLastName(lastName: String): QIO[List[User]] = ctx.run(userSchema.filter(_.lastName == lift(lastName)))

    override def list(): QIO[List[User]] = ctx.run(userSchema)

    override def userRoles(userId: UserId): QIO[List[Role]] = ctx.run(userSchema
      .join(userToRoleSchema).on({case (user, userToRole) => user.id == userToRole.userId})
      .join(roleSchema).on({case ((user, userToRole), role) => userToRole.roleId == role.code}))
      .map(r => r.map(_._2))

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = ctx.run(userToRoleSchema.insert(_.roleId -> lift(roleCode.code), _.userId -> lift(userId.id))).unit

    override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = ctx.run(userSchema
        .join(userToRoleSchema).on({case (user, userToRole) => user.id == userToRole.userId})
        .join(roleSchema).on({case ((user, userToRole), role) => userToRole.roleId == role.code})
        .filter({case ((user, userToRole), role) => role.code == lift(roleCode.code)}))
      .map(r => r.map(_._1._1))

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] = ctx.run(roleSchema.filter(_.code == lift(roleCode.code))).map(_.headOption)

    override def createRole(role: Role): QIO[Role] = ctx.run(roleSchema.insertValue(lift(role)).returning(r => r))
}

object UserRepository{

    val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}