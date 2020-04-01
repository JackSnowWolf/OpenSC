class User < ActiveRecord::Base
    # has_secure_password(validations: false)
    # has_secure_password :recovery_password, validations: false
    has_secure_password

    # enables @user.notes
    # This enables that, If you delete a user, its associated notes will also be deleted.
    has_many :notes, dependent: :destroy

    validates :password_digest, length: { minimum: 6 }
    validates :email, uniqueness: true

end
