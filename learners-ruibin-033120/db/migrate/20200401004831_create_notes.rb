class CreateNotes < ActiveRecord::Migration[6.0]
  def change
    create_table :notes do |t|
      t.references :user,  foreign_key: true #null: false,
      t.string :title
      t.text :body
      t.references :subject, foreign_key: true #null: false, 

      t.timestamps
    end
  end
end
